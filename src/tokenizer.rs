use crate::settings::TokenizerSettings;
use crate::token::{Token, TokenType};
use crate::trie::{Trie, TrieResult};

#[derive(Debug)]
struct Tokenizer {
    sql: Vec<char>,
    size: usize,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    comments: Vec<String>,
    is_end: bool,
    current_char: char,
    peek_char: char,
    previous_token_line: Option<usize>,
    keyword_trie: Trie,
    settings: TokenizerSettings,
}

impl Tokenizer {
    fn from_sql(sql: &str, settings: Option<TokenizerSettings>) -> Tokenizer {
        let settings = settings.unwrap_or(TokenizerSettings::default());
        let keyword_trie = Trie::empty();
        Tokenizer {
            sql: sql.chars().collect(),
            size: sql.len(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            comments: Vec::new(),
            is_end: false,
            current_char: '\0',
            peek_char: '\0',
            previous_token_line: None,
            keyword_trie,
            settings,
        }
    }

    fn scan<F>(&mut self, until: Option<F>)
    where
        F: Fn() -> bool,
    {
        while self.size > 0 && !self.is_end {
            self.start = self.current;
            self.advance(1, false, false);

            if self.current_char == '\0' {
                break;
            }

            if !self.settings.white_space.contains_key(&self.current_char) {
                if self.current_char.is_digit(10) {
                    self.scan_number();
                }
                if self.settings.identifiers.contains_key(&self.current_char) {
                    self.scan_identifier(
                        &self
                            .settings
                            .identifiers
                            .get(&self.current_char)
                            .unwrap()
                            .to_string() as &str,
                    );
                } else {
                    self.scan_keyword();
                }
            }

            if until.is_some() && until.as_ref().unwrap()() {
                break;
            }
        }
        if !self.tokens.is_empty() && !self.comments.is_empty() {
            self.tokens
                .last_mut()
                .unwrap()
                .append_comments(&mut self.comments);
        }
    }

    fn advance(&mut self, i: usize, alnum: bool, backtrack: bool) {
        match self.settings.white_space.get(&self.current_char) {
            Some(TokenType::BREAK) => {
                self.column = 1;
                self.line += 1;
            }
            _ => {
                if !backtrack {
                    self.column += i
                } else {
                    self.column -= i
                }
            }
        }

        if !backtrack {
            self.current += i
        } else {
            self.current -= i
        };

        self.is_end = self.current >= self.size;
        self.current_char = self.char_at(self.current - 1);
        self.peek_char = if self.is_end {
            '\0'
        } else {
            self.char_at(self.current)
        };

        if alnum && self.current_char.is_alphanumeric() {
            let mut column = self.column;
            let mut current = self.current;
            let mut peek_char = self.peek_char;
            let mut is_end = self.is_end;

            while peek_char.is_alphanumeric() {
                column += 1;
                current += 1;
                is_end = current >= self.size;
                peek_char = if is_end { '\0' } else { self.char_at(current) };
            }

            self.column = column;
            self.current = current;
            self.is_end = is_end;
            self.peek_char = peek_char;
            self.current_char = self.char_at(self.current - 1);
        }
    }

    fn peek(&self, i: usize) -> char {
        let index = self.current + i;
        if index < self.size {
            self.char_at(index)
        } else {
            '\0'
        }
    }

    fn chars(&self, size: usize) -> &[char] {
        let start = self.current - 1;
        let end = start + size;
        if end <= self.size {
            &self.sql[start..end]
        } else {
            &[]
        }
    }

    fn char_at(&self, index: usize) -> char {
        *self.sql.get(index).unwrap()
    }

    fn text(&self) -> String {
        self.sql[self.start..self.current].iter().collect()
    }

    fn add(&mut self, token_type: TokenType, text: Option<String>) {
        self.previous_token_line = Some(self.line);

        if !self.comments.is_empty()
            && !self.tokens.is_empty()
            && token_type == TokenType::SEMICOLON
        {
            self.tokens
                .last_mut()
                .unwrap()
                .append_comments(&mut self.comments);
        }

        self.tokens.push(Token::new(
            token_type,
            text.unwrap_or(self.text()),
            self.line,
            self.column,
            self.start,
            self.current - 1,
            std::mem::replace(&mut self.comments, Vec::new()),
        ));

        // If we have either a semicolon or a begin token before the command's token, we'll parse
        // whatever follows the command's token as a string.
        // FIXME: Add this logic.
    }

    fn scan_keyword(&mut self) {
        let mut size: usize = 0;
        let mut word: Option<String> = None;
        let mut chars = self.text();
        let mut current_char = '\0';
        let mut prev_space = false;
        let mut skip = false;
        let mut is_single_token = chars.len() == 1
            && self
                .settings
                .single_tokens
                .contains_key(&chars.chars().next().unwrap());

        let (mut trie_result, mut trie_node) =
            self.keyword_trie.root.contains(&chars.to_uppercase());

        while !chars.is_empty() {
            match trie_result {
                TrieResult::Failed => break,
                TrieResult::Exists => word = Some(chars.clone()),
                _ => {}
            }

            let end = self.current + size;
            size += 1;

            if end < self.size {
                current_char = self.char_at(end);
                is_single_token =
                    is_single_token || self.settings.single_tokens.contains_key(&current_char);
                let is_space = self.settings.white_space.contains_key(&current_char);

                if !is_space || !prev_space {
                    if is_space {
                        current_char = ' ';
                    }
                    chars.push(current_char);
                    prev_space = is_space;
                    skip = false;
                } else {
                    skip = true;
                }
            } else {
                current_char = '\0';
                chars = String::from(" ");
            }

            if skip {
                trie_result = TrieResult::Prefix;
            } else {
                (trie_result, trie_node) =
                    trie_node.contains(&current_char.to_uppercase().collect::<String>());
            }
        }

        if word.is_some() {
            let unwrapped_word = word.unwrap();
            if self.scan_string(&unwrapped_word) {
                return;
            }
            if self.scan_comment(&unwrapped_word) {
                return;
            }
            if prev_space || is_single_token || current_char == '\0' {
                self.advance(size - 1, false, false);
                let normalized_word = unwrapped_word.to_uppercase();
                self.add(
                    *self
                        .settings
                        .keywords
                        .get(&normalized_word as &str)
                        .unwrap(),
                    Some(unwrapped_word),
                );
            }
        }

        if self.settings.single_tokens.contains_key(&self.current_char) {
            self.add(
                *self.settings.single_tokens.get(&self.current_char).unwrap(),
                Some(self.current_char.to_string()),
            );
            return;
        }

        self.scan_var();
    }

    fn scan_comment(&mut self, start: &str) -> bool {
        false
    }

    fn scan_string(&mut self, start: &str) -> bool {
        false
    }

    fn scan_number(&mut self) {
        if self.current_char == '0' {
            let peek_char = self.peek_char.to_ascii_uppercase();
            if peek_char == 'B' {
                // FIXME: Binary
            } else if peek_char == 'X' {
                // FIXME: Hex
            }
        }

        let mut decimal = false;
        let mut scientific = 0;

        loop {
            if self.peek_char.is_digit(10) {
                self.advance(1, false, false);
            } else if self.peek_char == '.' && !decimal {
                let after = self.peek(1);
                if after.is_digit(10) || !after.is_alphabetic() {
                    decimal = true;
                    self.advance(1, false, false);
                } else {
                    self.add(TokenType::VAR, None);
                    return;
                }
            } else if (self.peek_char == '-' || self.peek_char == '+') && scientific == 1 {
                scientific += 1;
                self.advance(1, false, false);
            } else if self.peek_char.to_ascii_uppercase() == 'E' && scientific == 0 {
                scientific += 1;
                self.advance(1, false, false);
            } else if self.peek_char.is_alphabetic() || self.peek_char == '_' {
                let number_text = self.text();
                let mut literal = String::from("");

                while !self.peek_char.is_whitespace()
                    && !self.settings.white_space.contains_key(&self.peek_char)
                {
                    literal.push(self.peek_char);
                    self.advance(1, false, false);
                }

                let token_type = self
                    .settings
                    .keywords
                    .get(
                        self.settings
                            .numeric_literals
                            .get(&literal.to_uppercase() as &str)
                            .unwrap_or(&""),
                    )
                    .map(|x| *x);

                if token_type.is_some() {
                    self.add(TokenType::NUMBER, Some(number_text));
                    self.add(TokenType::DCOLON, Some("::".to_string()));
                    self.add(token_type.unwrap(), Some(literal));
                    return;
                } else if self.settings.identifiers_can_start_with_digit {
                    self.add(TokenType::VAR, None);
                    return;
                }

                self.advance(literal.len(), false, true);
                self.add(TokenType::NUMBER, Some(number_text));
                return;
            } else {
                self.add(TokenType::NUMBER, None);
                return;
            }
        }
    }

    fn scan_bits(&mut self) {}

    fn scan_hex(&mut self) {}

    fn scan_var(&mut self) {
        loop {
            let peek_char = if !self.peek_char.is_whitespace() {
                self.peek_char
            } else {
                '\0'
            };
            if peek_char != '\0'
                && (self.settings.var_single_tokens.contains(&peek_char)
                    || !self.settings.single_tokens.contains_key(&peek_char))
            {
                self.advance(1, true, false);
            } else {
                break;
            }
        }

        let token_type = if self.tokens.last().map(|t| t.token_type) == Some(TokenType::PARAMETER) {
            TokenType::VAR
        } else {
            self.settings
                .keywords
                .get(&self.text().to_uppercase() as &str)
                .map(|x| *x)
                .unwrap_or(TokenType::VAR)
        };
        self.add(token_type, None);
    }

    fn scan_identifier(&mut self, identifier_end: &str) {
        self.advance(1, false, false);
        let text = self.extract_string(identifier_end, true);
        self.add(TokenType::IDENTIFIER, Some(text));
    }

    fn extract_string(&mut self, delimiter: &str, use_identifier_escapes: bool) -> String {
        let mut text = String::from("");
        let peek_char_str = self.peek_char.to_string();

        loop {
            let escapes = if use_identifier_escapes {
                &self.settings.identifier_escapes
            } else {
                &self.settings.string_escapes
            };

            if escapes.contains(&self.current_char)
                && (peek_char_str == delimiter || escapes.contains(&self.peek_char))
                && (!self.settings.quotes.contains_key(&self.current_char)
                    || self.current_char == self.peek_char)
            {
                if peek_char_str == delimiter {
                    text.push(self.peek_char);
                } else {
                    text.push(self.current_char);
                    text.push(self.peek_char);
                }
                if self.current + 1 < self.size {
                    self.advance(2, false, false);
                } else {
                    panic!("Missing {} from {}:{}", delimiter, self.line, self.current);
                }
            } else {
                if self.chars(delimiter.len()).iter().collect::<String>() == delimiter {
                    if delimiter.len() > 1 {
                        self.advance(delimiter.len() - 1, false, false);
                    }
                    break;
                }
                if self.is_end {
                    panic!("Missing {} from {}:{}", delimiter, self.line, self.current);
                }
                // FIXME: Support escape sequences

                let current = self.current - 1;
                self.advance(1, true, false);
                text.push_str(
                    &self.sql[current..self.current - 1]
                        .iter()
                        .collect::<String>() as &str,
                );
            }
        }
        return text;
    }
}

#[cfg(test)]
mod test {

    use crate::tokenizer::Tokenizer;

    #[test]
    fn test_basic() {
        let mut tokenizer = Tokenizer::from_sql("select a, b, 1, 2", None);
        tokenizer.scan::<fn() -> bool>(None);
        dbg!(tokenizer.tokens);
    }
}
