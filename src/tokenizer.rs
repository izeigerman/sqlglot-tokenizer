use crate::settings::TokenizerSettings;
use crate::token::{Token, TokenType};
use crate::trie::{Trie, TrieResult};

#[derive(Debug)]
pub struct Tokenizer {
    settings: TokenizerSettings,
    keyword_trie: Trie,
}

impl Tokenizer {
    pub fn new(settings: Option<TokenizerSettings>) -> Tokenizer {
        Tokenizer {
            settings: settings.unwrap_or(TokenizerSettings::default()),
            keyword_trie: Trie::new(),
        }
    }

    pub fn tokenize(&self, sql: &str) -> Vec<Token> {
        let mut state = TokenizerState::new(sql, &self.settings, &self.keyword_trie);
        state.tokenize()
    }
}

#[derive(Debug)]
struct TokenizerState<'a> {
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
    keyword_trie: &'a Trie,
    settings: &'a TokenizerSettings,
}

impl<'a> TokenizerState<'a> {
    fn new(
        sql: &str,
        settings: &'a TokenizerSettings,
        keyword_trie: &'a Trie,
    ) -> TokenizerState<'a> {
        TokenizerState {
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

    fn tokenize(&mut self) -> Vec<Token> {
        self.scan::<fn() -> bool>(None);
        std::mem::replace(&mut self.tokens, Vec::new())
    }

    fn scan<F>(&mut self, until: Option<&F>)
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
                } else {
                    match self.settings.identifiers.get(&self.current_char) {
                        Some(identifier_end) => self.scan_identifier(&identifier_end.to_string()),
                        None => self.scan_keyword(),
                    }
                }
            }

            if until.map(|f| f()).unwrap_or(false) {
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
            while self.peek_char.is_alphanumeric() {
                self.column += 1;
                self.current += 1;
                self.is_end = self.current >= self.size;
                self.peek_char = if self.is_end {
                    '\0'
                } else {
                    self.char_at(self.current)
                };
            }
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
                    *self.settings.keywords.get(&normalized_word).unwrap(),
                    Some(unwrapped_word),
                );
                return;
            }
        }

        match self.settings.single_tokens.get(&self.current_char) {
            Some(token_type) => self.add(*token_type, Some(self.current_char.to_string())),
            None => self.scan_var(),
        }
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
                            .get(&literal.to_uppercase())
                            .unwrap_or(&String::from("")),
                    )
                    .map(|x| *x);

                match token_type {
                    Some(unwrapped_token_type) => {
                        self.add(TokenType::NUMBER, Some(number_text));
                        self.add(TokenType::DCOLON, Some("::".to_string()));
                        self.add(unwrapped_token_type, Some(literal));
                    }
                    None if self.settings.identifiers_can_start_with_digit => {
                        self.add(TokenType::VAR, None);
                    }
                    None => {
                        self.advance(literal.len(), false, true);
                        self.add(TokenType::NUMBER, Some(number_text));
                    }
                }
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
                .get(&self.text().to_uppercase())
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
                    // FIXME: use Result instead of panic
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
                    // FIXME: use Result instead of panic
                }
                // FIXME: Support escape sequences

                let current = self.current - 1;
                self.advance(1, true, false);
                text.push_str(
                    &self.sql[current..self.current - 1]
                        .iter()
                        .collect::<String>(),
                );
            }
        }
        return text;
    }
}

#[cfg(test)]
mod test {

    use std::time::Instant;

    use crate::tokenizer::Tokenizer;

    #[test]
    fn test_basic() {
        let input = r#"
    select
    'abcdef',
    12345
    from def.ght
    GROUP BY z,
    2, 3 union ALL
"#
        .repeat(100);
        let num_runs = 1000;
        let mut total_micros: u32 = 0;
        let tokenizer = Tokenizer::new(None);
        for _ in 0..num_runs {
            let start = Instant::now();
            tokenizer.tokenize(&input);
            let elapsed = start.elapsed();
            total_micros += elapsed.as_micros() as u32;
        }
        dbg!(total_micros / num_runs);
    }
}
