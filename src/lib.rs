use base64::{Engine as _, engine::general_purpose};
use chrono::{DateTime, Utc};
use memchr::memmem::Finder;
use pulldown_cmark::{Options, Parser, html};
use rand::{RngExt};
use regex::Regex;
use std::collections::HashMap;
use std::sync::LazyLock;
use std::sync::{Arc, Mutex, OnceLock};

use ulid::Ulid;
use unicode_segmentation::UnicodeSegmentation;
use unidecode::unidecode;
use uuid::Uuid;

// Type aliases for complex types
type UuidSequence = Arc<Mutex<Option<(Vec<Uuid>, usize)>>>;
type UlidSequence = Arc<Mutex<Option<(Vec<Ulid>, usize)>>>;
type RandomSequence = Arc<Mutex<Option<(Vec<String>, usize)>>>;

// Global caches for performance
static SNAKE_CACHE: LazyLock<Arc<Mutex<HashMap<String, String>>>> =
    LazyLock::new(|| Arc::new(Mutex::new(HashMap::new())));
static CAMEL_CACHE: LazyLock<Arc<Mutex<HashMap<String, String>>>> =
    LazyLock::new(|| Arc::new(Mutex::new(HashMap::new())));
static STUDLY_CACHE: LazyLock<Arc<Mutex<HashMap<String, String>>>> =
    LazyLock::new(|| Arc::new(Mutex::new(HashMap::new())));

// Factory functions
static UUID_FACTORY: OnceLock<Box<dyn Fn() -> Uuid + Send + Sync>> = OnceLock::new();
static ULID_FACTORY: OnceLock<Box<dyn Fn() -> Ulid + Send + Sync>> = OnceLock::new();
static RANDOM_STRING_FACTORY: OnceLock<Box<dyn Fn(usize) -> String + Send + Sync>> =
    OnceLock::new();

// Sequence factories
static UUID_SEQUENCE: LazyLock<UuidSequence> = LazyLock::new(|| Arc::new(Mutex::new(None)));
static ULID_SEQUENCE: LazyLock<UlidSequence> = LazyLock::new(|| Arc::new(Mutex::new(None)));
static RANDOM_SEQUENCE: LazyLock<RandomSequence> = LazyLock::new(|| Arc::new(Mutex::new(None)));

// Invisible characters constant
const INVISIBLE_CHARACTERS: &str = "\u{0009}\u{0020}\u{00A0}\u{00AD}\u{034F}\u{061C}\u{115F}\u{1160}\u{17B4}\u{17B5}\u{180E}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200A}\u{200B}\u{200C}\u{200D}\u{200E}\u{200F}\u{202F}\u{205F}\u{2060}\u{2061}\u{2062}\u{2063}\u{2064}\u{2065}\u{206A}\u{206B}\u{206C}\u{206D}\u{206E}\u{206F}\u{3000}\u{2800}\u{3164}\u{FEFF}\u{FFA0}\u{1D159}\u{1D173}\u{1D174}\u{1D175}\u{1D176}\u{1D177}\u{1D178}\u{1D179}\u{1D17A}\u{E0020}";

// Pluralization rules (simplified English rules)
static PLURAL_RULES: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
    vec![
        // Most specific / irregular first
        (Regex::new(r"(?i)^(child)$").unwrap(), "children"),
        (Regex::new(r"(?i)^(man)$").unwrap(), "men"),
        (Regex::new(r"(?i)^(woman)$").unwrap(), "women"),
        (Regex::new(r"(?i)^(person)$").unwrap(), "people"),
        (Regex::new(r"(?i)^(foot)$").unwrap(), "feet"),
        (Regex::new(r"(?i)^(tooth)$").unwrap(), "teeth"),
        (Regex::new(r"(?i)^(medium)$").unwrap(), "media"),
        (Regex::new(r"(?i)^(niveau)$").unwrap(), "niveaux"),
        (Regex::new(r"(?i)^(hippopotamus)$").unwrap(), "hippopotami"),
        (Regex::new(r"(?i)^(die)$").unwrap(), "dice"),
        (Regex::new(r"(?i)^(axis)$").unwrap(), "axes"),
        (Regex::new(r"(?i)^(appendix)$").unwrap(), "appendices"),
        (Regex::new(r"(?i)^(phenomenon)$").unwrap(), "phenomena"),
        // ox → oxen
        (Regex::new(r"(?i)^(ox)$").unwrap(), "${1}en"),
        // virus, stimulus, ...
        (Regex::new(r"(?i)(alumn|bacill|cact|foc|fung|nucle|radi|stimul|syllab|termin|vir)us$").unwrap(), "${1}i"),
        (Regex::new(r"(?i)(octop|vir)us$").unwrap(), "${1}i"), // octopus, virus (also covered above)
        // matrix, index, vertex → -ices
        (Regex::new(r"(?i)(matr|vert|ind)(ix|ex)$").unwrap(), "${1}ices"),
        // analysis, basis, crisis → -ses
        (Regex::new(r"(?i)(analys|ax|cris|test|thes)is$").unwrap(), "${1}es"),
        (Regex::new(r"(?i)^(analy)sis$").unwrap(), "${1}ses"),
        // criteria, phenomena
        (Regex::new(r"(?i)(criteri|phenomen)on$").unwrap(), "${1}a"),
        (Regex::new(r"(?i)(curricul|memorand)um$").unwrap(), "${1}a"),
        // tomato, potato, hero, ...
        (Regex::new(r"(?i)(buffal|her|potat|tomat|volcan|ech|torped|vet)o$").unwrap(), "${1}oes"),
        // quiz → quizzes
        (Regex::new(r"(?i)(quiz)$").unwrap(), "${1}zes"),
        // leaf, loaf, thief → -ves
        (Regex::new(r"(?i)(?:([^f])fe|([lr])f)$").unwrap(), "${1}${2}ves"),
        (Regex::new(r"(?i)(shea|loa|lea|thie)f$").unwrap(), "${1}ves"),
        // knife → knives (but safe → safes)
        (Regex::new(r"(?i)([^s])fe$").unwrap(), "${1}ves"),
        // mouse → mice, louse → lice
        (Regex::new(r"(?i)([ml])ouse$").unwrap(), "${1}ice"),
        // Ends in x, ch, ss, sh → +es
        (Regex::new(r"(?i)(x|ch|ss|sh)$").unwrap(), "${1}es"),
        // Words ending in consonant + y → ies
        (Regex::new(r"(?i)([^aeiouy]|qu)y$").unwrap(), "${1}ies"),
        // status → statuses
        (Regex::new(r"(?i)(status)$").unwrap(), "${1}es"),
        (Regex::new(r"(?i)(alias)$").unwrap(), "${1}es"),
        // bus → buses
        (Regex::new(r"(?i)(bu)s$").unwrap(), "${1}ses"),
        // Most generic plural rules (last)
        (Regex::new(r"(?i)s$").unwrap(), "s"),     // glasses, buses (already handled), etc.
        (Regex::new(r"(?i)$").unwrap(), "s"),      // default add -s
    ]
});

static SINGULAR_RULES: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
    vec![
        // Most specific first — reverse order philosophy
        (Regex::new(r"(?i)children$").unwrap(), "child"),
        (Regex::new(r"(?i)men$").unwrap(), "man"),
        (Regex::new(r"(?i)people$").unwrap(), "person"),
        (Regex::new(r"(?i)feet$").unwrap(), "foot"),
        (Regex::new(r"(?i)teeth$").unwrap(), "tooth"),
        (Regex::new(r"(?i)geese$").unwrap(), "goose"),
        (Regex::new(r"(?i)oxen$").unwrap(), "ox"),
        (Regex::new(r"(?i)^(media)$").unwrap(), "medium"),
        (Regex::new(r"(?i)^(niveaux)$").unwrap(), "niveau"),
        (Regex::new(r"(?i)^(hippopotami)$").unwrap(), "hippopotamus"),
        (Regex::new(r"(?i)^(dice)$").unwrap(), "die"),
        (Regex::new(r"(?i)^(axes)$").unwrap(), "axis"),
        (Regex::new(r"(?i)^(appendices)$").unwrap(), "appendix"),
        (Regex::new(r"(?i)^(phenomena)$").unwrap(), "phenomenon"),
        // ox → oxen
        // -i → -us
        (Regex::new(r"(?i)(alumn|bacill|cact|foc|fung|nucle|radi|stimul|syllab|termin|vir)i$").unwrap(), "${1}us"),
        (Regex::new(r"(?i)(octop|vir)i$").unwrap(), "${1}us"),
        // -ices → -ix / -ex
        (Regex::new(r"(?i)(matr)ices$").unwrap(), "${1}ix"),
        (Regex::new(r"(?i)(vert|ind)ices$").unwrap(), "${1}ex"),
        // -ses → -sis
        (Regex::new(r"(?i)(analy|ba|diagno|parenthe|progno|synop|the)ses$").unwrap(), "${1}sis"),
        (Regex::new(r"(?i)(cris|test|ax|thes)es$").unwrap(), "${1}is"),
        // -a → -um / -on
        (Regex::new(r"(?i)(criteri|phenomen)a$").unwrap(), "${1}on"),
        (Regex::new(r"(?i)([ti])a$").unwrap(), "${1}um"),
        // -oes → -o
        (Regex::new(r"(?i)(buffal|her|potat|tomat|volcan|ech|torped|vet)oes$").unwrap(), "${1}o"),
        // quizzes → quiz
        (Regex::new(r"(?i)(quiz)zes$").unwrap(), "${1}"),
        // -ves → -f / -fe
        (Regex::new(r"(?i)([^f])ves$").unwrap(), "${1}fe"),
        (Regex::new(r"(?i)([lr])ves$").unwrap(), "${1}f"),
        (Regex::new(r"(?i)(shea|loa|lea|thie)ves$").unwrap(), "${1}f"),
        // mice, lice
        (Regex::new(r"(?i)([ml])ice$").unwrap(), "${1}ouse"),
        // -xes, -ches, -shes, -sses
        (Regex::new(r"(?i)(x|ch|ss|sh)es$").unwrap(), "${1}"),
        // consonant + ies → y
        (Regex::new(r"(?i)([^aeiouy]|qu)ies$").unwrap(), "${1}y"),
        // statuses, aliases
        (Regex::new(r"(?i)(status|alias)es$").unwrap(), "${1}"),
        (Regex::new(r"(?i)(bu)ses$").unwrap(), "${1}s"),
        // Most generic
        (Regex::new(r"(?i)(.*)s$").unwrap(), "${1}"), // remove final s (fallback)
    ]
});

pub struct Str;

impl Str {
    /// Return the remainder of a string after the first occurrence of a given value
    pub fn after(subject: &str, search: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.find(search) {
            subject[(pos + search.len())..].to_string()
        } else {
            subject.to_string()
        }
    }

    /// Return the remainder of a string after the last occurrence of a given value
    pub fn after_last(subject: &str, search: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.rfind(search) {
            subject[(pos + search.len())..].to_string()
        } else {
            subject.to_string()
        }
    }

    /// Transliterate a UTF-8 value to ASCII
    pub fn ascii(value: &str) -> String {
        unidecode(value)
    }

    /// Transliterate a string to its closest ASCII representation
    pub fn transliterate(string: &str, unknown: Option<&str>, strict: Option<bool>) -> String {
        let unknown = unknown.unwrap_or("?");
        let strict = strict.unwrap_or(false);

        string
            .chars()
            .map(|c| {
                if c.is_ascii() {
                    c.to_string()
                } else {
                    let transliterated = unidecode(&c.to_string());
                    if transliterated.is_empty()
                        || (strict && !transliterated.chars().all(|ch| ch.is_ascii_alphanumeric()))
                    {
                        unknown.to_string()
                    } else {
                        transliterated
                    }
                }
            })
            .collect()
    }

    /// Get the portion of a string before the first occurrence of a given value
    pub fn before(subject: &str, search: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.find(search) {
            subject[..pos].to_string()
        } else {
            subject.to_string()
        }
    }

    /// Get the portion of a string before the last occurrence of a given value
    pub fn before_last(subject: &str, search: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.rfind(search) {
            subject[..pos].to_string()
        } else {
            subject.to_string()
        }
    }

    /// Get the portion of a string between two given values
    pub fn between(subject: &str, from: &str, to: &str) -> String {
        if from.is_empty() || to.is_empty() {
            return subject.to_string();
        }

        let after_from = Self::after(subject, from);
        Self::before_last(&after_from, to)
    }

    /// Get the smallest possible portion of a string between two given values
    pub fn between_first(subject: &str, from: &str, to: &str) -> String {
        if from.is_empty() || to.is_empty() {
            return subject.to_string();
        }

        let after_from = Self::after(subject, from);
        Self::before(&after_from, to)
    }

    /// Convert a value to camel case
    pub fn camel(value: &str) -> String {
        let cache = CAMEL_CACHE.lock().unwrap();
        if let Some(cached) = cache.get(value) {
            return cached.clone();
        }
        drop(cache);

        let studly = Self::studly(value);
        let result = Self::lcfirst(&studly);

        let mut cache = CAMEL_CACHE.lock().unwrap();
        cache.insert(value.to_string(), result.clone());
        result
    }

    /// Get the character at the specified index
    pub fn char_at(subject: &str, index: isize) -> Option<char> {
        let chars: Vec<char> = subject.chars().collect();
        let length = chars.len() as isize;

        let actual_index = if index < 0 {
            if index < -length {
                return None;
            }
            (length + index) as usize
        } else {
            if index >= length {
                return None;
            }
            index as usize
        };

        chars.get(actual_index).copied()
    }

    /// Remove the given string(s) if it exists at the start of the haystack
    pub fn chop_start(subject: &str, needles: &[&str]) -> String {
        for needle in needles {
            if let Some(stripped) = subject.strip_prefix(needle) {
                return stripped.to_string();
            }
        }
        subject.to_string()
    }

    /// Remove the given string(s) if it exists at the end of the haystack
    pub fn chop_end(subject: &str, needles: &[&str]) -> String {
        for needle in needles {
            if let Some(stripped) = subject.strip_suffix(needle) {
                return stripped.to_string();
            }
        }
        subject.to_string()
    }

    /// Determine if a given string contains a given substring
    pub fn contains(haystack: &str, needles: &[&str], ignore_case: bool) -> bool {
        let haystack_check = if ignore_case {
            &haystack.to_lowercase()
        } else {
            haystack
        };

        for needle in needles.iter() {
            if needle.is_empty() {
                continue;
            }

            let needle_check = if ignore_case {
                &needle.to_lowercase()
            } else {
                *needle
            };

            let finder = Finder::new(&needle_check);
            if finder.find(haystack_check.as_bytes()).is_some() {
                return true;
            }
        }
        false
    }

    /// Determine if a given string contains all array values
    pub fn contains_all(haystack: &str, needles: &[&str], ignore_case: bool) -> bool {
        for needle in needles {
            if !Self::contains(haystack, &[needle], ignore_case) {
                return false;
            }
        }
        true
    }

    /// Determine if a given string doesn't contain a given substring
    pub fn doesnt_contain(haystack: &str, needles: &[&str], ignore_case: bool) -> bool {
        !Self::contains(haystack, needles, ignore_case)
    }

    /// Convert the case of a string
    pub fn convert_case(string: &str, mode: CaseMode) -> String {
        match mode {
            CaseMode::Upper => string.to_uppercase(),
            CaseMode::Lower => string.to_lowercase(),
            CaseMode::Title => Self::title(string),
            CaseMode::Fold => string.to_lowercase(), // Case folding approximation
        }
    }

    /// Replace consecutive instances of a given character with a single character
    pub fn deduplicate(string: &str, characters: &[char]) -> String {
        let mut result = string.to_string();

        for &character in characters {
            let pattern = format!("{}{{2,}}", regex::escape(&character.to_string()));
            let re = Regex::new(&pattern).unwrap();
            result = re.replace_all(&result, &character.to_string()).to_string();
        }

        result
    }

    /// Determine if a given string ends with a given substring
    pub fn ends_with(haystack: &str, needles: &[&str]) -> bool {
        for needle in needles {
            if !needle.is_empty() && haystack.ends_with(needle) {
                return true;
            }
        }
        false
    }

    /// Determine if a given string doesn't end with a given substring
    pub fn doesnt_end_with(haystack: &str, needles: &[&str]) -> bool {
        !Self::ends_with(haystack, needles)
    }

    /// Extracts an excerpt from text that matches the first instance of a phrase
    pub fn excerpt(text: &str, phrase: &str, options: ExcerptOptions) -> Option<String> {
        if phrase.is_empty() {
            return None;
        }

        let re = Regex::new(&format!(r"(?i)^(.*?)({})(.*)$", regex::escape(phrase))).ok()?;
        let caps = re.captures(text)?;

        let before = caps.get(1)?.as_str().trim_start();
        let matched = caps.get(2)?.as_str();
        let after = caps.get(3)?.as_str().trim_end();

        let start_chars: Vec<char> = before.chars().collect();
        let start_len = start_chars.len();
        let start_excerpt = if start_len > options.radius {
            let excerpt_start = start_len - options.radius;
            let excerpt: String = start_chars.iter().skip(excerpt_start).collect();
            format!("{}{}", options.omission, excerpt.trim_start())
        } else {
            before.to_string()
        };

        let end_chars: Vec<char> = after.chars().collect();
        let end_excerpt = if end_chars.len() > options.radius {
            let excerpt: String = end_chars.iter().take(options.radius).collect();
            format!("{}{}", excerpt.trim_end(), options.omission)
        } else {
            after.to_string()
        };

        Some(format!("{}{}{}", start_excerpt, matched, end_excerpt))
    }

    /// Cap a string with a single instance of a given value
    pub fn finish(value: &str, cap: &str) -> String {
        let pattern = format!("({})+$", regex::escape(cap));
        let re = Regex::new(&pattern).unwrap();
        let trimmed = re.replace_all(value, "");
        format!("{}{}", trimmed, cap)
    }

    /// Wrap the string with the given strings
    pub fn wrap(value: &str, before: &str, after: Option<&str>) -> String {
        let after = after.unwrap_or(before);
        format!("{}{}{}", before, value, after)
    }

    /// Unwrap the string with the given strings
    pub fn unwrap(value: &str, before: &str, after: Option<&str>) -> String {
        let after = after.unwrap_or(before);
        let mut result = value.to_string();

        if result.starts_with(before) {
            result = result[before.len()..].to_string();
        }

        if result.ends_with(after) {
            result = result[..result.len() - after.len()].to_string();
        }

        result
    }

    /// Determine if a given string matches a given pattern (with wildcards)
    pub fn is_pattern(patterns: &[&str], value: &str, ignore_case: bool) -> bool {
        for pattern in patterns {
            if *pattern == "*" || *pattern == value {
                return true;
            }

            if ignore_case && pattern.to_lowercase() == value.to_lowercase() {
                return true;
            }

            let escaped = regex::escape(pattern);
            let pattern_regex = escaped.replace("\\*", ".*");
            let flags = if ignore_case { "(?i)" } else { "" };
            let full_pattern = format!("{}^{}$", flags, pattern_regex);

            if let Ok(re) = Regex::new(&full_pattern)
                && re.is_match(value)
            {
                return true;
            }
        }
        false
    }

    /// Determine if a given string is 7 bit ASCII
    pub fn is_ascii(value: &str) -> bool {
        value.is_ascii()
    }

    /// Determine if a given value is valid JSON
    pub fn is_json(value: &str) -> bool {
        serde_json::from_str::<serde_json::Value>(value).is_ok()
    }

    /// Determine if a given value is a valid URL
    pub fn is_url(value: &str, protocols: Option<&[&str]>) -> bool {
        if let Ok(parsed) = url::Url::parse(value) {
            if let Some(allowed_protocols) = protocols {
                allowed_protocols.contains(&parsed.scheme())
            } else {
                true
            }
        } else {
            false
        }
    }

    /// Determine if a given value is a valid UUID
    pub fn is_uuid(value: &str, version: Option<UuidVersion>) -> bool {
        if let Ok(uuid) = Uuid::parse_str(value) {
            match version {
                Some(UuidVersion::Nil) => uuid.is_nil(),
                Some(UuidVersion::V1) => uuid.get_version() == Some(uuid::Version::Mac),
                Some(UuidVersion::V3) => uuid.get_version() == Some(uuid::Version::Md5),
                Some(UuidVersion::V4) => uuid.get_version() == Some(uuid::Version::Random),
                Some(UuidVersion::V5) => uuid.get_version() == Some(uuid::Version::Sha1),
                Some(UuidVersion::Max) => uuid.is_max(),
                None => true,
            }
        } else {
            false
        }
    }

    /// Determine if a given value is a valid ULID
    pub fn is_ulid(value: &str) -> bool {
        Ulid::from_string(value).is_ok()
    }

    /// Convert a string to kebab case
    pub fn kebab(value: &str) -> String {
        Self::snake(value, "-")
    }

    /// Return the length of the given string
    pub fn length(value: &str) -> usize {
        value.graphemes(true).count()
    }

    /// Limit the number of characters in a string
    pub fn limit(value: &str, limit: usize, end: &str, preserve_words: bool) -> String {
        let graphemes: Vec<&str> = value.graphemes(true).collect();

        if graphemes.len() <= limit {
            return value.to_string();
        }

        if !preserve_words {
            let truncated: String = graphemes.iter().take(limit).copied().collect();
            return format!("{}{}", truncated, end);
        }

        // For word preservation, we need to work with the actual string
        let truncated: String = graphemes.iter().take(limit).copied().collect();
        let trimmed = truncated.trim_end();

        // Find the last space and truncate there
        if let Some(last_space) = trimmed.rfind(' ') {
            format!("{}{}", &trimmed[..last_space], end)
        } else {
            format!("{}{}", trimmed, end)
        }
    }

    /// Convert the given string to lower-case
    pub fn lower(value: &str) -> String {
        value.to_lowercase()
    }

    /// Limit the number of words in a string
    pub fn words(value: &str, words: usize, end: &str) -> String {
        let word_vec: Vec<&str> = value.split_whitespace().collect();

        if word_vec.len() <= words {
            return value.to_string();
        }

        let truncated: Vec<&str> = word_vec.iter().take(words).copied().collect();
        format!("{}{}", truncated.join(" "), end)
    }

    /// Converts GitHub flavored Markdown into HTML
    pub fn markdown(string: &str, options: MarkdownOptions) -> String {
        let mut md_options = Options::empty();
        if options.tables {
            md_options.insert(Options::ENABLE_TABLES);
        }
        if options.footnotes {
            md_options.insert(Options::ENABLE_FOOTNOTES);
        }
        if options.strikethrough {
            md_options.insert(Options::ENABLE_STRIKETHROUGH);
        }
        if options.tasklists {
            md_options.insert(Options::ENABLE_TASKLISTS);
        }

        let parser = Parser::new_ext(string, md_options);
        let mut html_output = String::new();
        html::push_html(&mut html_output, parser);
        html_output
    }

    /// Converts inline Markdown into HTML
    pub fn inline_markdown(string: &str, options: MarkdownOptions) -> String {
        // For inline markdown, we strip block elements
        let html = Self::markdown(string, options);
        let re = Regex::new(r"</?(?:p|div|h[1-6]|blockquote|pre|ul|ol|li)[^>]*>").unwrap();
        re.replace_all(&html, "").to_string()
    }

    /// Masks a portion of a string with a repeated character
    pub fn mask(string: &str, character: char, index: isize, length: Option<usize>) -> String {
        if character == '\0' {
            return string.to_string();
        }

        let chars: Vec<char> = string.chars().collect();
        let str_len = chars.len() as isize;

        let start_index = if index < 0 {
            if index < -str_len {
                0
            } else {
                (str_len + index) as usize
            }
        } else {
            if index >= str_len {
                return string.to_string();
            }
            index as usize
        };

        let mask_length = length.unwrap_or(chars.len() - start_index);
        let end_index = std::cmp::min(start_index + mask_length, chars.len());

        let start: String = chars.iter().take(start_index).collect();
        let mask: String = character.to_string().repeat(end_index - start_index);
        let end: String = chars.iter().skip(end_index).collect();

        format!("{}{}{}", start, mask, end)
    }

    /// Get the string matching the given pattern
    pub fn match_pattern(pattern: &str, subject: &str) -> String {
        if let Ok(re) = Regex::new(pattern) {
            if let Some(caps) = re.captures(subject) {
                if let Some(group1) = caps.get(1) {
                    group1.as_str().to_string()
                } else if let Some(group0) = caps.get(0) {
                    group0.as_str().to_string()
                } else {
                    String::new()
                }
            } else {
                String::new()
            }
        } else {
            String::new()
        }
    }

    /// Determine if a given string matches a given pattern
    pub fn is_match(patterns: &[&str], value: &str) -> bool {
        for pattern in patterns {
            if let Ok(re) = Regex::new(pattern)
                && re.is_match(value)
            {
                return true;
            }
        }
        false
    }

    /// Get all strings matching the given pattern
    pub fn match_all(pattern: &str, subject: &str) -> Vec<String> {
        if let Ok(re) = Regex::new(pattern) {
            re.find_iter(subject)
                .map(|m| m.as_str().to_string())
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Remove all non-numeric characters from a string
    pub fn numbers(value: &str) -> String {
        value.chars().filter(|c| c.is_numeric()).collect()
    }

    /// Pad both sides of a string with another
    pub fn pad_both(value: &str, length: usize, pad: &str) -> String {
        let current_len = Self::length(value);
        if current_len >= length {
            return value.to_string();
        }

        let total_padding = length - current_len;
        let left_padding = total_padding / 2;
        let right_padding = total_padding - left_padding;

        let left_pad = pad.repeat(left_padding.div_ceil(pad.len()));
        let right_pad = pad.repeat(right_padding.div_ceil(pad.len()));

        format!(
            "{}{}{}",
            &left_pad[..left_padding.min(left_pad.len())],
            value,
            &right_pad[..right_padding.min(right_pad.len())]
        )
    }

    /// Pad the left side of a string with another
    pub fn pad_left(value: &str, length: usize, pad: &str) -> String {
        let current_len = Self::length(value);
        if current_len >= length {
            return value.to_string();
        }

        let padding_needed = length - current_len;
        let pad_string = pad.repeat(padding_needed.div_ceil(pad.len()));

        format!(
            "{}{}",
            &pad_string[..padding_needed.min(pad_string.len())],
            value
        )
    }

    /// Pad the right side of a string with another
    pub fn pad_right(value: &str, length: usize, pad: &str) -> String {
        let current_len = Self::length(value);
        if current_len >= length {
            return value.to_string();
        }

        let padding_needed = length - current_len;
        let pad_string = pad.repeat(padding_needed.div_ceil(pad.len()));

        format!(
            "{}{}",
            value,
            &pad_string[..padding_needed.min(pad_string.len())]
        )
    }

    /// Parse a Class[@]method style callback into class and method
    pub fn parse_callback(callback: &str, default: Option<&str>) -> (String, Option<String>) {
        if callback.contains("@anonymous") {
            let at_count = callback.matches('@').count();
            if at_count > 1 {
                let parts: Vec<&str> = callback.rsplitn(2, '@').collect();
                return (parts[1].to_string(), Some(parts[0].to_string()));
            }
            return (callback.to_string(), default.map(|s| s.to_string()));
        }

        if callback.contains('@') {
            let parts: Vec<&str> = callback.splitn(2, '@').collect();
            (parts[0].to_string(), Some(parts[1].to_string()))
        } else {
            (callback.to_string(), default.map(|s| s.to_string()))
        }
    }

    /// Get the plural form of an English word
    pub fn plural(value: &str, count: i32, prepend_count: bool) -> String {
        let plural_form = if count == 1 {
            value.to_string()
        } else {
            for (rule, replacement) in PLURAL_RULES.iter() {
                if rule.is_match(value) {
                    return if prepend_count {
                        format!("{} {}", count, rule.replace(value, *replacement))
                    } else {
                        rule.replace(value, *replacement).to_string()
                    };
                }
            }
            format!("{}s", value) // fallback
        };

        if prepend_count {
            format!("{} {}", count, plural_form)
        } else {
            plural_form
        }
    }

    /// Pluralize the last word of an English, studly caps case string
    pub fn plural_studly(value: &str, count: i32) -> String {
        let re = Regex::new(r"([a-z])([A-Z])").unwrap();
        let with_spaces = re.replace_all(value, "$1 $2");
        let parts: Vec<&str> = with_spaces.split_whitespace().collect();

        if let Some(last_word) = parts.last() {
            let mut result = parts[..parts.len() - 1].join("");
            result.push_str(&Self::plural(last_word, count, false));
            result
        } else {
            Self::plural(value, count, false)
        }
    }

    /// Pluralize the last word of an English, Pascal caps case string
    pub fn plural_pascal(value: &str, count: i32) -> String {
        Self::plural_studly(value, count)
    }

    /// Generate a random, secure password
    pub fn password(
        length: usize,
        letters: bool,
        numbers: bool,
        symbols: bool,
        spaces: bool,
    ) -> String {
        let mut chars = Vec::new();

        if letters {
            chars.extend_from_slice(&[
                'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
                'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                'W', 'X', 'Y', 'Z',
            ]);
        }

        if numbers {
            chars.extend_from_slice(&['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
        }

        if symbols {
            chars.extend_from_slice(&[
                '~', '!', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '.', ',', '<', '>',
                '?', '/', '\\', '{', '}', '[', ']', '|', ':', ';',
            ]);
        }

        if spaces {
            chars.push(' ');
        }

        if chars.is_empty() {
            return String::new();
        }

        let mut rng = rand::rng();
        (0..length)
            .map(|_| chars[rng.random_range(0..chars.len())])
            .collect()
    }

    /// Find the multi-byte safe position of the first occurrence of a given substring
    pub fn position(haystack: &str, needle: &str, offset: usize) -> Option<usize> {
        let chars: Vec<char> = haystack.chars().collect();
        if offset >= chars.len() {
            return None;
        }

        let search_slice: String = chars.iter().skip(offset).collect();
        if let Some(pos) = search_slice.find(needle) {
            let chars_before_match: String = chars.iter().take(offset).collect();
            Some(chars_before_match.len() + pos)
        } else {
            None
        }
    }

    /// Generate a more truly "random" alpha-numeric string
    pub fn random(length: usize) -> String {
        // Check for sequence first
        {
            let mut sequence = RANDOM_SEQUENCE.lock().unwrap();
            if let Some((seq, index)) = sequence.as_mut()
                && *index < seq.len()
            {
                let result = seq[*index].clone();
                *index += 1;
                return result;
            }
        }

        if let Some(factory) = RANDOM_STRING_FACTORY.get() {
            return factory(length);
        }

        let charset: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        let mut rng = rand::rng();

        (0..length)
            .map(|_| {
                let idx = rng.random_range(0..charset.len());
                charset[idx] as char
            })
            .collect()
    }

    /// Set the callable that will be used to generate random strings
    pub fn create_random_strings_using<F>(factory: F)
    where
        F: Fn(usize) -> String + Send + Sync + 'static,
    {
        let _ = RANDOM_STRING_FACTORY.set(Box::new(factory));
    }

    /// Set the sequence that will be used to generate random strings
    pub fn create_random_strings_using_sequence(sequence: Vec<String>) {
        let mut seq_guard = RANDOM_SEQUENCE.lock().unwrap();
        *seq_guard = Some((sequence, 0));
    }

    /// Indicate that random strings should be created normally
    pub fn create_random_strings_normally() {
        // Clear the factory and sequence
        // Note: OnceLock doesn't have a clear method, so we'd need to restructure
        // For now, we'll clear the sequence
        let mut seq_guard = RANDOM_SEQUENCE.lock().unwrap();
        *seq_guard = None;
    }

    /// Repeat the given string
    pub fn repeat(string: &str, times: usize) -> String {
        string.repeat(times)
    }

    /// Replace a given value in the string sequentially with an array
    pub fn replace_array(search: &str, replace: &[&str], subject: &str) -> String {
        let segments: Vec<&str> = subject.split(search).collect();
        let mut result = segments[0].to_string();

        for (i, segment) in segments.iter().skip(1).enumerate() {
            let replacement = replace.get(i).unwrap_or(&search);
            result.push_str(replacement);
            result.push_str(segment);
        }

        result
    }

    /// Replace the given value in the given string
    pub fn replace(
        search: &[&str],
        replace: &[&str],
        subject: &str,
        case_sensitive: bool,
    ) -> String {
        let mut result = subject.to_string();

        // Use a unique placeholder to avoid double replacement
        let mut placeholders = Vec::new();

        for (i, &search_term) in search.iter().enumerate() {
            let placeholder = format!("__PLACEHOLDER_{}__", i);
            placeholders.push(placeholder.clone());

            if case_sensitive {
                result = result.replace(search_term, &placeholder);
            } else {
                let re = Regex::new(&format!("(?i){}", regex::escape(search_term))).unwrap();
                result = re.replace_all(&result, placeholder.as_str()).to_string();
            }
        }

        // Replace placeholders with actual replacements
        for (i, placeholder) in placeholders.iter().enumerate() {
            let replacement = replace.get(i).unwrap_or(&"");
            result = result.replace(placeholder, replacement);
        }

        result
    }

    /// Replace the first occurrence of a given value in the string
    pub fn replace_first(search: &str, replace: &str, subject: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.find(search) {
            let mut result = String::new();
            result.push_str(&subject[..pos]);
            result.push_str(replace);
            result.push_str(&subject[pos + search.len()..]);
            result
        } else {
            subject.to_string()
        }
    }

    /// Replace the first occurrence of the given value if it appears at the start
    pub fn replace_start(search: &str, replace: &str, subject: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if subject.starts_with(search) {
            Self::replace_first(search, replace, subject)
        } else {
            subject.to_string()
        }
    }

    /// Replace the last occurrence of a given value in the string
    pub fn replace_last(search: &str, replace: &str, subject: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if let Some(pos) = subject.rfind(search) {
            let mut result = String::new();
            result.push_str(&subject[..pos]);
            result.push_str(replace);
            result.push_str(&subject[pos + search.len()..]);
            result
        } else {
            subject.to_string()
        }
    }

    /// Replace the last occurrence of a given value if it appears at the end
    pub fn replace_end(search: &str, replace: &str, subject: &str) -> String {
        if search.is_empty() {
            return subject.to_string();
        }

        if subject.ends_with(search) {
            Self::replace_last(search, replace, subject)
        } else {
            subject.to_string()
        }
    }

    /// Replace the patterns matching the given regular expression
    pub fn replace_matches(
        pattern: &str,
        replace: &str,
        subject: &str,
        limit: Option<usize>,
    ) -> String {
        if let Ok(re) = Regex::new(pattern) {
            match limit {
                Some(n) => re.replacen(subject, n, replace).to_string(),
                None => re.replace_all(subject, replace).to_string(),
            }
        } else {
            subject.to_string()
        }
    }

    /// Remove any occurrence of the given string in the subject
    pub fn remove(search: &[&str], subject: &str, case_sensitive: bool) -> String {
        let empty_replace: Vec<&str> = search.iter().map(|_| "").collect();
        Self::replace(search, &empty_replace, subject, case_sensitive)
    }

    /// Reverse the given string
    pub fn reverse(value: &str) -> String {
        value.chars().rev().collect()
    }

    /// Begin a string with a single instance of a given value
    pub fn start(value: &str, prefix: &str) -> String {
        let pattern = format!("^({})+", regex::escape(prefix));
        let re = Regex::new(&pattern).unwrap();
        let trimmed = re.replace_all(value, "");
        format!("{}{}", prefix, trimmed)
    }

    /// Convert the given string to upper-case
    pub fn upper(value: &str) -> String {
        value.to_uppercase()
    }

    /// Convert the given string to title case
    pub fn title(value: &str) -> String {
        value
            .split_whitespace()
            .map(Self::ucfirst)
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Convert the given string to headline case
    pub fn headline(value: &str) -> String {
        let re = Regex::new(r"\s+").unwrap();
        let words: Vec<&str> = re.split(value).collect();

        let processed: Vec<String> = if words.len() > 1 {
            words.iter().map(|word| Self::title(word)).collect()
        } else {
            Self::uc_split(value)
                .iter()
                .map(|word| Self::title(word))
                .collect()
        };

        let collapsed = Self::replace(
            &["-", "_", " "],
            &["_", "_", "_"],
            &processed.join("_"),
            true,
        );
        let parts: Vec<&str> = collapsed.split('_').filter(|s| !s.is_empty()).collect();
        parts.join(" ")
    }

    /// Convert the given string to APA-style title case
    pub fn apa(value: &str) -> String {
        if value.trim().is_empty() {
            return value.to_string();
        }

        let minor_words = [
            "and", "as", "but", "for", "if", "nor", "or", "so", "yet", "a", "an", "the", "at",
            "by", "for", "in", "of", "off", "on", "per", "to", "up", "via",
        ];

        let end_punctuation = ['.', '!', '?', ':', '—', ','];
        let words: Vec<&str> = value.split_whitespace().collect();

        let mut result = Vec::new();

        for (i, word) in words.iter().enumerate() {
            let lowercase_word = word.to_lowercase();

            if word.contains('-') {
                let hyphenated: Vec<&str> = word.split('-').collect();
                let processed: Vec<String> = hyphenated
                    .iter()
                    .map(|part| {
                        let lower_part = part.to_lowercase();
                        if minor_words.contains(&lower_part.as_str()) && part.len() <= 3 {
                            lower_part
                        } else {
                            Self::ucfirst(part)
                        }
                    })
                    .collect();
                result.push(processed.join("-"));
            } else if minor_words.contains(&lowercase_word.as_str())
                && word.len() <= 3
                && i > 0
                && !end_punctuation.contains(&words[i - 1].chars().last().unwrap_or(' '))
            {
                result.push(lowercase_word);
            } else {
                result.push(Self::ucfirst(word));
            }
        }

        result.join(" ")
    }

    /// Get the singular form of an English word
    pub fn singular(value: &str) -> String {
        for (rule, replacement) in SINGULAR_RULES.iter() {
            if rule.is_match(value) {
                return rule.replace(value, *replacement).to_string();
            }
        }
        value.to_string()
    }

    /// Generate a URL friendly "slug" from a given string
    pub fn slug(
        title: &str,
        separator: &str,
        language: Option<&str>,
        dictionary: Option<HashMap<String, String>>,
    ) -> String {
        let mut title = if language.is_some() {
            Self::ascii(title)
        } else {
            title.to_string()
        };

        // Convert all dashes/underscores into separator
        let flip = if separator == "-" { "_" } else { "-" };
        let pattern = format!("[{}]+", regex::escape(flip));
        let re = Regex::new(&pattern).unwrap();
        title = re.replace_all(&title, separator).to_string();

        // Replace dictionary words
        if let Some(dict) = dictionary {
            for (key, value) in dict {
                let replacement = format!("{}{}{}", separator, value, separator);
                title = title.replace(&key, &replacement);
            }
        } else {
            // Default dictionary
            let replacement = format!("{}at{}", separator, separator);
            title = title.replace("@", &replacement);
        }

        // Remove all characters that are not the separator, letters, numbers, or whitespace
        let pattern = format!("[^{}\\p{{L}}\\p{{N}}\\s]+", regex::escape(separator));
        let re = Regex::new(&pattern).unwrap();
        title = re.replace_all(&Self::lower(&title), "").to_string();

        // Replace all separator characters and whitespace by a single separator
        let pattern = format!("[{}\\s]+", regex::escape(separator));
        let re = Regex::new(&pattern).unwrap();
        title = re.replace_all(&title, separator).to_string();

        title
            .trim_matches(separator.chars().next().unwrap_or('-'))
            .to_string()
    }

    /// Convert a string to snake case
    pub fn snake(value: &str, delimiter: &str) -> String {
        let cache_key = format!("{}:{}", value, delimiter);
        let cache = SNAKE_CACHE.lock().unwrap();
        if let Some(cached) = cache.get(&cache_key) {
            return cached.clone();
        }
        drop(cache);

        if value
            .chars()
            .all(|c| c.is_lowercase() || !c.is_alphabetic())
        {
            let mut cache = SNAKE_CACHE.lock().unwrap();
            cache.insert(cache_key, value.to_string());
            return value.to_string();
        }

        // Replace spaces with delimiter, then handle camelCase
        let no_spaces = value.replace(' ', delimiter);

        let re = Regex::new(r"([a-z])([A-Z])").unwrap();
        let with_delim = re.replace_all(&no_spaces, |caps: &regex::Captures| {
            let first = &caps[1];
            let second = &caps[2];

            format!("{}{}{}", first, delimiter, second)
        });

        let result = with_delim.to_lowercase();

        let mut cache = SNAKE_CACHE.lock().unwrap();
        cache.insert(cache_key, result.clone());
        result
    }

    /// Remove all whitespace from both ends of a string
    pub fn trim(value: &str) -> String {
        Self::trim_custom(value, None)
    }

    /// Remove all whitespace from both ends of a string with custom character list
    pub fn trim_custom(value: &str, charlist: Option<&str>) -> String {
        match charlist {
            Some(chars) => value.trim_matches(|c| chars.contains(c)).to_string(),
            None => {
                let invisible_chars: Vec<char> = INVISIBLE_CHARACTERS.chars().collect();
                let mut result = value.to_string();

                // Trim from start
                while let Some(ch) = result.chars().next() {
                    if ch.is_whitespace() || invisible_chars.contains(&ch) {
                        result = result[ch.len_utf8()..].to_string();
                    } else {
                        break;
                    }
                }

                // Trim from end
                while let Some(ch) = result.chars().last() {
                    if ch.is_whitespace() || invisible_chars.contains(&ch) {
                        let mut chars = result.chars();
                        chars.next_back();
                        result = chars.as_str().to_string();
                    } else {
                        break;
                    }
                }

                result
            }
        }
    }

    /// Remove all whitespace from the beginning of a string
    pub fn ltrim(value: &str, charlist: Option<&str>) -> String {
        match charlist {
            Some(chars) => value.trim_start_matches(|c| chars.contains(c)).to_string(),
            None => {
                let invisible_chars: Vec<char> = INVISIBLE_CHARACTERS.chars().collect();
                let mut result = value.to_string();

                while let Some(ch) = result.chars().next() {
                    if ch.is_whitespace() || invisible_chars.contains(&ch) {
                        result = result[ch.len_utf8()..].to_string();
                    } else {
                        break;
                    }
                }

                result
            }
        }
    }

    /// Remove all whitespace from the end of a string
    pub fn rtrim(value: &str, charlist: Option<&str>) -> String {
        match charlist {
            Some(chars) => value.trim_end_matches(|c| chars.contains(c)).to_string(),
            None => {
                let invisible_chars: Vec<char> = INVISIBLE_CHARACTERS.chars().collect();
                let mut result = value.to_string();

                while let Some(ch) = result.chars().last() {
                    if ch.is_whitespace() || invisible_chars.contains(&ch) {
                        let mut chars = result.chars();
                        chars.next_back();
                        result = chars.as_str().to_string();
                    } else {
                        break;
                    }
                }

                result
            }
        }
    }

    /// Remove all "extra" blank space from the given string
    pub fn squish(value: &str) -> String {
        let trimmed = Self::trim(value);
        let re = Regex::new(r"\s+").unwrap();
        re.replace_all(&trimmed, " ").to_string()
    }

    /// Determine if a given string starts with a given substring
    pub fn starts_with(haystack: &str, needles: &[&str]) -> bool {
        for needle in needles {
            if !needle.is_empty() && haystack.starts_with(needle) {
                return true;
            }
        }
        false
    }

    /// Determine if a given string doesn't start with a given substring
    pub fn doesnt_start_with(haystack: &str, needles: &[&str]) -> bool {
        !Self::starts_with(haystack, needles)
    }

    /// Convert a value to studly caps case
    pub fn studly(value: &str) -> String {
        let cache = STUDLY_CACHE.lock().unwrap();
        if let Some(cached) = cache.get(value) {
            return cached.clone();
        }
        drop(cache);

        let cleaned = value.replace(['-', '_'], " ");
        let words: Vec<&str> = cleaned.split_whitespace().collect();
        let result: String = words.iter().map(|word| Self::ucfirst(word)).collect();

        let mut cache = STUDLY_CACHE.lock().unwrap();
        cache.insert(value.to_string(), result.clone());
        result
    }

    /// Convert a value to Pascal case (alias for studly)
    pub fn pascal(value: &str) -> String {
        Self::studly(value)
    }

    /// Returns the portion of the string specified by the start and length parameters
    pub fn substr(string: &str, start: isize, length: Option<usize>) -> String {
        let chars: Vec<char> = string.chars().collect();
        let str_len = chars.len() as isize;

        let start_index = if start < 0 {
            if start < -str_len {
                0
            } else {
                (str_len + start) as usize
            }
        } else {
            if start >= str_len {
                return String::new();
            }
            start as usize
        };

        match length {
            Some(len) => {
                let end_index = std::cmp::min(start_index + len, chars.len());
                chars
                    .iter()
                    .skip(start_index)
                    .take(end_index - start_index)
                    .collect()
            }
            None => chars.iter().skip(start_index).collect(),
        }
    }

    /// Returns the number of substring occurrences
    pub fn substr_count(
        haystack: &str,
        needle: &str,
        offset: usize,
        length: Option<usize>,
    ) -> usize {
        let search_slice: String = match length {
            Some(len) => {
                let chars: Vec<char> = haystack.chars().collect();
                let end_index = std::cmp::min(offset + len, chars.len());
                if offset >= chars.len() {
                    return 0;
                }
                chars.iter().skip(offset).take(end_index - offset).collect()
            }
            None => {
                let chars: Vec<char> = haystack.chars().collect();
                if offset >= chars.len() {
                    return 0;
                }
                chars.iter().skip(offset).collect()
            }
        };

        search_slice.matches(needle).count()
    }

    /// Replace text within a portion of a string
    pub fn substr_replace(
        string: &str,
        replace: &str,
        offset: isize,
        length: Option<usize>,
    ) -> String {
        let chars: Vec<char> = string.chars().collect();
        let str_len = chars.len() as isize;

        let start_index = if offset < 0 {
            if offset < -str_len {
                0
            } else {
                (str_len + offset) as usize
            }
        } else {
            if offset >= str_len {
                return format!("{}{}", string, replace);
            }
            offset as usize
        };

        let replace_length = length.unwrap_or(chars.len() - start_index);
        let end_index = std::cmp::min(start_index + replace_length, chars.len());

        let start: String = chars.iter().take(start_index).collect();
        let end: String = chars.iter().skip(end_index).collect();

        format!("{}{}{}", start, replace, end)
    }

    /// Swap multiple keywords in a string with other keywords
    pub fn swap(map: HashMap<String, String>, subject: &str) -> String {
        let mut result = subject.to_string();
        for (search, replace) in map {
            result = result.replace(&search, &replace);
        }
        result
    }

    /// Take the first or last {$limit} characters of a string
    pub fn take(string: &str, limit: isize) -> String {
        if limit < 0 {
            Self::substr(string, limit, None)
        } else {
            Self::substr(string, 0, Some(limit as usize))
        }
    }

    /// Convert the given string to Base64 encoding
    pub fn to_base64(string: &str) -> String {
        general_purpose::STANDARD.encode(string.as_bytes())
    }

    /// Decode the given Base64 encoded string
    pub fn from_base64(string: &str, strict: bool) -> Result<String, base64::DecodeError> {
        let engine = if strict {
            general_purpose::STANDARD_NO_PAD
        } else {
            general_purpose::STANDARD
        };

        let bytes = engine.decode(string)?;
        Ok(String::from_utf8_lossy(&bytes).to_string())
    }

    /// Make a string's first character lowercase
    pub fn lcfirst(string: &str) -> String {
        let mut chars = string.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_lowercase().collect::<String>() + chars.as_str(),
        }
    }

    /// Make a string's first character uppercase
    pub fn ucfirst(string: &str) -> String {
        let mut chars = string.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    }

    /// Split a string into pieces by uppercase characters
    pub fn uc_split(string: &str) -> Vec<String> {
        let mut result = Vec::new();
        let mut current = String::new();

        for ch in string.chars() {
            if ch.is_uppercase() && !current.is_empty() {
                result.push(current);
                current = String::new();
            }
            current.push(ch);
        }

        if !current.is_empty() {
            result.push(current);
        }

        result
    }

    /// Get the number of words a string contains
    pub fn word_count(string: &str, characters: Option<&str>) -> usize {
        if let Some(chars) = characters {
            // Custom word character definition
            let pattern = format!("[{}\\s]+", regex::escape(chars));
            let re = Regex::new(&pattern).unwrap();
            re.split(string).filter(|s| !s.is_empty()).count()
        } else {
            string.split_whitespace().count()
        }
    }

    /// Wrap a string to a given number of characters
    pub fn word_wrap(
        string: &str,
        characters: usize,
        break_str: &str,
        cut_long_words: bool,
    ) -> String {
        if cut_long_words {
            textwrap::fill(string, characters)
        } else {
            let options = textwrap::Options::new(characters).break_words(false);
            textwrap::fill(string, options)
        }
        .replace('\n', break_str)
    }

    /// Generate a UUID (version 4)
    pub fn uuid() -> Uuid {
        // Check for sequence first
        {
            let mut sequence = UUID_SEQUENCE.lock().unwrap();
            if let Some((seq, index)) = sequence.as_mut()
                && *index < seq.len()
            {
                let result = seq[*index];
                *index += 1;
                return result;
            }
        }

        if let Some(factory) = UUID_FACTORY.get() {
            return factory();
        }
        Uuid::new_v4()
    }

    /// Generate a UUID (version 7)
    pub fn uuid7(time: Option<DateTime<Utc>>) -> Uuid {
        if let Some(factory) = UUID_FACTORY.get() {
            return factory();
        }

        match time {
            Some(_) => Uuid::new_v4(), // Simplified - would need uuid v7 implementation
            None => Uuid::new_v4(),
        }
    }

    /// Generate a time-ordered UUID
    pub fn ordered_uuid() -> Uuid {
        if let Some(factory) = UUID_FACTORY.get() {
            return factory();
        }

        // Simplified implementation - in reality would use COMB UUID
        Uuid::new_v4()
    }

    /// Set the callable that will be used to generate UUIDs
    pub fn create_uuids_using<F>(factory: F)
    where
        F: Fn() -> Uuid + Send + Sync + 'static,
    {
        let _ = UUID_FACTORY.set(Box::new(factory));
    }

    /// Set the sequence that will be used to generate UUIDs
    pub fn create_uuids_using_sequence(sequence: Vec<Uuid>) {
        let mut seq_guard = UUID_SEQUENCE.lock().unwrap();
        *seq_guard = Some((sequence, 0));
    }

    /// Always return the same UUID when generating new UUIDs
    pub fn freeze_uuids<F>(callback: Option<F>) -> Uuid
    where
        F: FnOnce(Uuid),
    {
        let uuid = Uuid::new_v4();
        Self::create_uuids_using(move || uuid);

        if let Some(cb) = callback {
            cb(uuid);
            Self::create_uuids_normally();
        }

        uuid
    }

    /// Indicate that UUIDs should be created normally and not using a custom factory
    pub fn create_uuids_normally() {
        // Clear the sequence
        let mut seq_guard = UUID_SEQUENCE.lock().unwrap();
        *seq_guard = None;
    }

    /// Generate a ULID
    pub fn ulid(time: Option<DateTime<Utc>>) -> Ulid {
        // Check for sequence first
        {
            let mut sequence = ULID_SEQUENCE.lock().unwrap();
            if let Some((seq, index)) = sequence.as_mut()
                && *index < seq.len()
            {
                let result = seq[*index];
                *index += 1;
                return result;
            }
        }

        if let Some(factory) = ULID_FACTORY.get() {
            return factory();
        }

        match time {
            Some(_) => Ulid::new(), // ULID library handles time internally
            None => Ulid::new(),
        }
    }

    /// Indicate that ULIDs should be created normally and not using a custom factory
    pub fn create_ulids_normally() {
        let mut seq_guard = ULID_SEQUENCE.lock().unwrap();
        *seq_guard = None;
    }

    /// Set the callable that will be used to generate ULIDs
    pub fn create_ulids_using<F>(factory: F)
    where
        F: Fn() -> Ulid + Send + Sync + 'static,
    {
        let _ = ULID_FACTORY.set(Box::new(factory));
    }

    /// Set the sequence that will be used to generate ULIDs
    pub fn create_ulids_using_sequence(sequence: Vec<Ulid>) {
        let mut seq_guard = ULID_SEQUENCE.lock().unwrap();
        *seq_guard = Some((sequence, 0));
    }

    /// Always return the same ULID when generating new ULIDs
    pub fn freeze_ulids<F>(callback: Option<F>) -> Ulid
    where
        F: FnOnce(Ulid),
    {
        let ulid = Ulid::new();
        Self::create_ulids_using(move || ulid);

        if let Some(cb) = callback {
            cb(ulid);
            Self::create_ulids_normally();
        }

        ulid
    }

    /// Remove all strings from the casing caches
    pub fn flush_cache() {
        let mut snake_cache = SNAKE_CACHE.lock().unwrap();
        let mut camel_cache = CAMEL_CACHE.lock().unwrap();
        let mut studly_cache = STUDLY_CACHE.lock().unwrap();

        snake_cache.clear();
        camel_cache.clear();
        studly_cache.clear();
    }

    /// Split a string by the given delimiter.
    ///
    /// Behaves like PHP's `explode`:
    /// - Empty parts are preserved
    /// - If delimiter is empty, splits into Unicode scalar values
    ///
    /// # Example
    /// ```rust
    /// use illuminate_string::Str;
    /// let v = Str::explode("a,,b", ",");
    /// assert_eq!(v, vec!["a", "", "b"]);
    /// ```
    pub fn explode(s: &str, delimiter: &str) -> Vec<String> {
        if delimiter.is_empty() {
            return s.chars().map(|c| c.to_string()).collect();
        }

        // split preserves empty segments => same behavior
        s.split(delimiter).map(String::from).collect()
    }

    /// Join string parts using the given delimiter.
    ///
    /// Accepts any iterator of string-like values.
    ///
    /// # Example
    /// ```rust
    /// use illuminate_string::Str;
    /// let s = Str::implode("-", ["a", "b", "c"]);
    /// assert_eq!(s, "a-b-c");
    /// ```
    pub fn implode<I, S>(delimiter: &str, parts: I) -> String
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let mut iter = parts.into_iter();

        let first = match iter.next() {
            Some(v) => v,
            None => return String::new(),
        };

        let first_ref = first.as_ref();
        let remaining_lower = iter.size_hint().0;
        let capacity = first_ref.len().saturating_add(remaining_lower.saturating_mul(delimiter.len()));
        let mut out = String::with_capacity(capacity);
        out.push_str(first_ref);

        for part in iter {
            out.push_str(delimiter);
            out.push_str(part.as_ref());
        }

        out
    }

    /// Limit a string by number of words.
    ///
    /// Words are detected using Unicode whitespace.
    /// If the string exceeds the limit, `end` is appended.
    ///
    /// # Example
    /// ```rust
    /// use illuminate_string::Str;
    /// let s = Str::limit_words("hello world from rust", 2, "...");
    /// assert_eq!(s, "hello world...");
    /// ```
    pub fn limit_words(s: &str, count: usize, end: &str) -> String {
        if count == 0 {
            return end.to_owned();
        }

        let mut iter = s.split_whitespace();
        let mut out = String::new();

        for i in 0..count {
            match iter.next() {
                Some(word) => {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(word);
                }
                None => {
                    // Fewer words than the limit, so we are done.
                    break;
                }
            }
        }

        if iter.next().is_some() {
            out.push_str(end);
        }

        out
    }
}

// Enums and structs for various options

#[derive(Debug, Clone, Copy)]
pub enum CaseMode {
    Upper,
    Lower,
    Title,
    Fold,
}

#[derive(Debug, Clone)]
pub struct ExcerptOptions {
    pub radius: usize,
    pub omission: String,
}

impl Default for ExcerptOptions {
    fn default() -> Self {
        Self {
            radius: 100,
            omission: "...".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MarkdownOptions {
    pub tables: bool,
    pub footnotes: bool,
    pub strikethrough: bool,
    pub tasklists: bool,
}

impl Default for MarkdownOptions {
    fn default() -> Self {
        Self {
            tables: true,
            footnotes: true,
            strikethrough: true,
            tasklists: true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UuidVersion {
    Nil,
    V1,
    V3,
    V4,
    V5,
    Max,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_after() {
        assert_eq!(Str::after("This is my name", "This is"), " my name");
        assert_eq!(Str::after("App\\Class@method", "@"), "method");
    }

    #[test]
    fn test_after_last() {
        assert_eq!(Str::after_last("yvette@yvette@gmail.com", "@"), "gmail.com");
    }

    #[test]
    fn test_ascii() {
        assert_eq!(Str::ascii("ñ"), "n");
        assert_eq!(Str::ascii("üñíçødé"), "unicode");
    }

    #[test]
    fn test_before() {
        assert_eq!(Str::before("This is my name", "my name"), "This is ");
        assert_eq!(Str::before("App\\Class@method", "@"), "App\\Class");
    }

    #[test]
    fn test_before_last() {
        assert_eq!(
            Str::before_last("yvette@yvette@gmail.com", "@"),
            "yvette@yvette"
        );
    }

    #[test]
    fn test_between() {
        assert_eq!(Str::between("This is my name", "This", "name"), " is my ");
        assert_eq!(Str::between("App\\Class@method", "\\", "@"), "Class");
    }

    #[test]
    fn test_between_first() {
        assert_eq!(Str::between_first("[a] bc [d]", "[", "]"), "a");
    }

    #[test]
    fn test_camel() {
        assert_eq!(Str::camel("foo_bar"), "fooBar");
        assert_eq!(Str::camel("foo-bar"), "fooBar");
        assert_eq!(Str::camel("FooBar"), "fooBar");
    }

    #[test]
    fn test_char_at() {
        assert_eq!(Str::char_at("This is my name", 6), Some('s'));
        assert_eq!(Str::char_at("This is my name", -1), Some('e'));
        assert_eq!(Str::char_at("This is my name", 100), None);
    }

    #[test]
    fn test_contains() {
        assert!(Str::contains("This is my name", &["my"], false));
        assert!(!Str::contains("This is my name", &["hello"], false));
        assert!(Str::contains("This is my name", &["MY"], true));
    }

    #[test]
    fn test_contains_large() {
        let lorem_ipsum = include_str!("../lorem_ipsum.txt");
        assert!(Str::contains(
            lorem_ipsum,
            &[
                "Cras lobortis lorem ac efficitur egestas. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Mauris lacinia volutpat velit, pharetra tincidunt tortor vestibulum eu. Phasellus consectetur molestie nibh vitae ornare. Vivamus fringilla nunc facilisis velit commodo finibus."
            ],
            false
        ));
        assert!(Str::contains(
            lorem_ipsum,
            &[
                "Nulla facilisi. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae;"
            ],
            false
        ));
        assert!(Str::contains(
            lorem_ipsum,
            &[
                "Quisque in lectus dapibus, rhoncus massa non, sodales velit. Quisque laoreet leo sit amet lectus efficitur, ac facilisis sem ultrices. Praesent ullamcorper lectus et ultrices pulvinar. Praesent ac fringilla enim. Sed eu hendrerit lectus. Aenean maximus nulla ut faucibus rhoncus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Praesent metus mauris, fringilla et vestibulum eget, tempus vitae erat. Fusce pulvinar bibendum urna sit amet tempus. Vivamus luctus facilisis faucibus. Sed pharetra, elit nec porttitor bibendum, nulla nibh dictum lectus, quis elementum purus arcu vel magna. Vestibulum at neque justo. Morbi scelerisque tortor a elementum hendrerit. Cras dapibus sit amet libero consequat tincidunt."
            ],
            false
        ));
        assert!(Str::contains(lorem_ipsum, &[
            "Ut tellus risus, elementum sed mauris at, consequat luctus sapien. Suspendisse accumsan, nulla ut tristique ullamcorper, nisi lectus egestas nibh, efficitur imperdiet sem nisl ac nisi. In lacinia in mauris sed hendrerit. Praesent maximus imperdiet sem ut malesuada. Sed quis congue purus, vel condimentum odio. Aliquam erat volutpat. Integer vel nibh non ipsum gravida dignissim. Proin luctus, turpis ac aliquam ultricies, ligula arcu fermentum neque, in facilisis felis lectus sed libero. Mauris lobortis, nulla at convallis efficitur, mi quam finibus metus, eu tempus nisi magna ac ex. Nunc eget sapien aliquam, vestibulum nisl non, dictum tortor.

Vestibulum sollicitudin nisi sed metus vehicula, eu pulvinar enim faucibus. Praesent feugiat vulputate orci non ultrices. Maecenas scelerisque mauris eu purus cursus, ut sollicitudin eros tincidunt. Morbi eget libero tellus. Sed pulvinar in quam sit amet sagittis. Nam malesuada congue odio vel sagittis. Curabitur rhoncus mollis ante a elementum. Aliquam volutpat tempor turpis, at dapibus lectus auctor sit amet. Aliquam venenatis a elit quis euismod. Ut faucibus lacinia ante ac porttitor. Nullam malesuada ex purus, in convallis risus pulvinar id. Etiam ut dolor neque. Pellentesque sed orci ultricies, iaculis velit eu, molestie sapien."
        ], false))
    }

    #[test]
    fn test_contains_all() {
        assert!(Str::contains_all("This is my name", &["This", "my"], false));
        assert!(!Str::contains_all(
            "This is my name",
            &["This", "hello"],
            false
        ));
    }

    #[test]
    fn test_deduplicate() {
        assert_eq!(Str::deduplicate("Hello    World", &[' ']), "Hello World");
        assert_eq!(Str::deduplicate("aabbcc", &['a', 'b']), "abcc");
    }

    #[test]
    fn test_ends_with() {
        assert!(Str::ends_with("Hello World", &["World"]));
        assert!(!Str::ends_with("Hello World", &["Hello"]));
    }

    #[test]
    fn test_excerpt() {
        let options = ExcerptOptions::default();
        let result = Str::excerpt(
            "This is a very long string that contains the word Laravel somewhere in the middle",
            "Laravel",
            options,
        );
        assert!(result.is_some());
        assert!(result.unwrap().contains("Laravel"));
    }

    #[test]
    fn test_finish() {
        assert_eq!(Str::finish("this/string", "/"), "this/string/");
        assert_eq!(Str::finish("this/string/", "/"), "this/string/");
    }

    #[test]
    fn test_is_ascii() {
        assert!(Str::is_ascii("Taylor"));
        assert!(!Str::is_ascii("ñ"));
    }

    #[test]
    fn test_is_json() {
        assert!(Str::is_json(r#"{"name":"John","age":30}"#));
        assert!(!Str::is_json("not json"));
    }

    #[test]
    fn test_is_url() {
        assert!(Str::is_url("https://laravel.com", None));
        assert!(!Str::is_url("not a url", None));
    }

    #[test]
    fn test_is_uuid() {
        assert!(Str::is_uuid("a0a2a2d2-0b87-4a18-83f2-2529882be2de", None));
        assert!(!Str::is_uuid("not a uuid", None));
    }

    #[test]
    fn test_kebab() {
        assert_eq!(Str::kebab("fooBar"), "foo-bar");
    }

    #[test]
    fn test_length() {
        assert_eq!(Str::length("Laravel"), 7);
        assert_eq!(Str::length("😀😃😄😁"), 4);
    }

    #[test]
    fn test_limit() {
        assert_eq!(
            Str::limit(
                "The quick brown fox jumps over the lazy dog",
                20,
                "...",
                false
            ),
            "The quick brown fox ..."
        );
    }

    #[test]
    fn test_lower() {
        assert_eq!(Str::lower("LARAVEL"), "laravel");
    }

    #[test]
    fn test_mask() {
        assert_eq!(
            Str::mask("taylor@example.com", '*', 3, Some(10)),
            "tay**********e.com"
        );
    }

    #[test]
    fn test_numbers() {
        assert_eq!(Str::numbers("Laravel 9.0"), "90");
    }

    #[test]
    fn test_pad_left() {
        assert_eq!(Str::pad_left("James", 10, " "), "     James");
    }

    #[test]
    fn test_pad_right() {
        assert_eq!(Str::pad_right("James", 10, "-"), "James-----");
    }

    #[test]
    fn test_plural() {
        assert_eq!(Str::plural("car", 1, false), "car");
        assert_eq!(Str::plural("car", 2, false), "cars");
        assert_eq!(Str::plural("child", 2, false), "children");
        assert_eq!(Str::plural("man", 2, false), "men");
        assert_eq!(Str::plural("woman", 2, false), "women");
        assert_eq!(Str::plural("person", 2, false), "people");
        assert_eq!(Str::plural("thief", 2, false), "thieves");
        assert_eq!(Str::plural("foot", 2, false), "feet");
        assert_eq!(Str::plural("foot", 2, false), "feet");
        assert_eq!(Str::plural("medium", 2, false), "media");
        assert_eq!(Str::plural("hoax", 2, false), "hoaxes");
    }

    #[test]
    fn test_random() {
        let random1 = Str::random(10);
        let random2 = Str::random(10);
        assert_eq!(random1.len(), 10);
        assert_eq!(random2.len(), 10);
        assert_ne!(random1, random2);
    }

    #[test]
    fn test_remove() {
        assert_eq!(
            Str::remove(
                &["e"],
                "Peter Piper picked a peck of pickled peppers.",
                true
            ),
            "Ptr Pipr pickd a pck of pickld ppprs."
        );
    }

    #[test]
    fn test_replace() {
        assert_eq!(
            Str::replace(&["9.x", "10.x"], &["10.x", "11.x"], "Laravel 9.x", true),
            "Laravel 10.x"
        );
    }

    #[test]
    fn test_replace_first() {
        assert_eq!(
            Str::replace_first("the", "a", "the quick brown fox jumps over the lazy dog"),
            "a quick brown fox jumps over the lazy dog"
        );
    }

    #[test]
    fn test_replace_last() {
        assert_eq!(
            Str::replace_last("the", "a", "the quick brown fox jumps over the lazy dog"),
            "the quick brown fox jumps over a lazy dog"
        );
    }

    #[test]
    fn test_reverse() {
        assert_eq!(Str::reverse("Hello World"), "dlroW olleH");
    }

    #[test]
    fn test_singular() {
        assert_eq!(Str::singular("cars"), "car");
        assert_eq!(Str::singular("children"), "child");
    }

    #[test]
    fn test_slug() {
        assert_eq!(
            Str::slug("Laravel 5 Framework", "-", None, None),
            "laravel-5-framework"
        );
    }

    #[test]
    fn test_snake() {
        assert_eq!(Str::snake("fooBar", "_"), "foo_bar");
        assert_eq!(Str::snake("FooBar", "_"), "foo_bar");
    }

    #[test]
    fn test_start() {
        assert_eq!(Str::start("this/string", "/"), "/this/string");
        assert_eq!(Str::start("/this/string", "/"), "/this/string");
    }

    #[test]
    fn test_starts_with() {
        assert!(Str::starts_with("Hello World", &["Hello"]));
        assert!(!Str::starts_with("Hello World", &["World"]));
    }

    #[test]
    fn test_studly() {
        assert_eq!(Str::studly("foo_bar"), "FooBar");
        assert_eq!(Str::studly("foo-bar"), "FooBar");
    }

    #[test]
    fn test_substr() {
        assert_eq!(Str::substr("The Laravel Framework", 4, Some(7)), "Laravel");
        assert_eq!(
            Str::substr("The Laravel Framework", -9, Some(9)),
            "Framework"
        );
    }

    #[test]
    fn test_title() {
        assert_eq!(
            Str::title("a nice title uses the correct case"),
            "A Nice Title Uses The Correct Case"
        );
    }

    #[test]
    fn test_ucfirst() {
        assert_eq!(Str::ucfirst("foo bar"), "Foo bar");
    }

    #[test]
    fn test_upper() {
        assert_eq!(Str::upper("laravel"), "LARAVEL");
    }

    #[test]
    fn test_uuid() {
        let uuid1 = Str::uuid();
        let uuid2 = Str::uuid();
        assert_ne!(uuid1, uuid2);
        assert!(Str::is_uuid(&uuid1.to_string(), None));
    }

    #[test]
    fn test_word_count() {
        assert_eq!(Str::word_count("Hello, how are you?", None), 4);
    }

    #[test]
    fn test_words() {
        assert_eq!(
            Str::words("Perfectly balanced, as all things should be.", 3, ">>>"),
            "Perfectly balanced, as>>>"
        );
    }

    #[test]
    fn test_explode_basic() {
        let parts = Str::explode("a,b,,c", ",");
        assert_eq!(parts, vec!["a", "b", "", "c"]);
    }

    #[test]
    fn test_explode_empty_delim() {
        let parts = Str::explode("abč", "");
        // splits by chars (UTF-8 codepoints), not grapheme clusters
        assert_eq!(parts, vec!["a", "b", "č"]);
    }

    #[test]
    fn test_implode_basic() {
        let vec = vec!["a", "b", "c"];
        let s = Str::implode("-", vec);
        assert_eq!(s, "a-b-c");
    }

    #[test]
    fn test_limit_words_trim() {
        let s = "This   is  a   test string";
        let out = Str::limit_words(s, 3, "...");
        assert_eq!(out, "This is a...");
    }

    #[test]
    fn test_limit_words_no_trim() {
        let s = "Short text";
        let out = Str::limit_words(s, 5, "...");
        assert_eq!(out, s);
    }

    #[test]
    fn test_limit_zero() {
        let s = "Anything";
        let out = Str::limit_words(s, 0, "...");
        assert_eq!(out, "...");
    }

}
