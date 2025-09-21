# illuminate-string

A comprehensive Rust library for string manipulation, inspired by Laravel's Str helper class. This library provides a fluent, intuitive API for common string operations with full Unicode support.

## Features

- **Case Conversion**: camelCase, snake_case, kebab-case, PascalCase, Title Case
- **String Analysis**: Pattern matching, validation, character analysis
- **Text Processing**: Limiting, wrapping, trimming, padding
- **Markdown Support**: Convert Markdown to HTML
- **Pluralization**: English pluralization and singularization
- **UUID/ULID Generation**: Generate unique identifiers
- **Base64 Encoding/Decoding**: Safe string encoding
- **Advanced Operations**: Masking, excerpting, transliteration

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
illuminate-string = "0.1.0"
```

## Quick Start

```rust
use illuminate_string::Str;

fn main() {
    // Case conversion
    assert_eq!(Str::camel("foo_bar"), "fooBar");
    assert_eq!(Str::snake("fooBar", "_"), "foo_bar");
    assert_eq!(Str::kebab("fooBar"), "foo-bar");
    assert_eq!(Str::studly("foo_bar"), "FooBar");

    // String analysis
    assert!(Str::contains("Hello World", &["World"], false));
    assert!(Str::starts_with("Hello World", &["Hello"]));
    assert!(Str::ends_with("Hello World", &["World"]));

    // Text manipulation
    assert_eq!(Str::limit("The quick brown fox", 10, "...", false), "The quick ...");
    assert_eq!(Str::reverse("Hello"), "olleH");
    assert_eq!(Str::ucfirst("hello world"), "Hello world");

    // Pluralization
    assert_eq!(Str::plural("car", 2, false), "cars");
    assert_eq!(Str::singular("cars"), "car");

    // UUID generation
    let uuid = Str::uuid();
    println!("Generated UUID: {}", uuid);
}
```

## API Reference

### Case Conversion

| Method | Description | Example |
|--------|-------------|---------|
| `camel(value)` | Convert to camelCase | `foo_bar` → `fooBar` |
| `snake(value, delimiter)` | Convert to snake_case | `fooBar` → `foo_bar` |
| `kebab(value)` | Convert to kebab-case | `fooBar` → `foo-bar` |
| `studly(value)` | Convert to StudlyCase/PascalCase | `foo_bar` → `FooBar` |
| `title(value)` | Convert to Title Case | `hello world` → `Hello World` |
| `upper(value)` | Convert to UPPERCASE | `hello` → `HELLO` |
| `lower(value)` | Convert to lowercase | `HELLO` → `hello` |

### String Analysis

| Method | Description | Example |
|--------|-------------|---------|
| `contains(haystack, needles, ignore_case)` | Check if string contains any of the needles | |
| `contains_all(haystack, needles, ignore_case)` | Check if string contains all needles | |
| `starts_with(haystack, needles)` | Check if string starts with any needle | |
| `ends_with(haystack, needles)` | Check if string ends with any needle | |
| `is_ascii(value)` | Check if string is 7-bit ASCII | |
| `is_json(value)` | Check if string is valid JSON | |
| `is_url(value, protocols)` | Check if string is valid URL | |
| `is_uuid(value, version)` | Check if string is valid UUID | |
| `is_ulid(value)` | Check if string is valid ULID | |

### Text Processing

| Method | Description | Example |
|--------|-------------|---------|
| `limit(value, limit, end, preserve_words)` | Limit string length | |
| `words(value, words, end)` | Limit number of words | |
| `trim(value)` | Remove whitespace from both ends | |
| `ltrim(value, charlist)` | Remove whitespace from start | |
| `rtrim(value, charlist)` | Remove whitespace from end | |
| `pad_left(value, length, pad)` | Pad string on the left | |
| `pad_right(value, length, pad)` | Pad string on the right | |
| `pad_both(value, length, pad)` | Pad string on both sides | |

### String Manipulation

| Method | Description | Example |
|--------|-------------|---------|
| `after(subject, search)` | Get string after first occurrence | |
| `after_last(subject, search)` | Get string after last occurrence | |
| `before(subject, search)` | Get string before first occurrence | |
| `before_last(subject, search)` | Get string before last occurrence | |
| `between(subject, from, to)` | Get string between two values | |
| `replace(search, replace, subject, case_sensitive)` | Replace occurrences | |
| `replace_first(search, replace, subject)` | Replace first occurrence | |
| `replace_last(search, replace, subject)` | Replace last occurrence | |
| `remove(search, subject, case_sensitive)` | Remove occurrences | |

### Advanced Features

#### Markdown Processing
```rust
use string_rs::{Str, MarkdownOptions};

let options = MarkdownOptions::default();
let html = Str::markdown("# Hello\nThis is **bold**", options);
```

#### String Masking
```rust
let masked = Str::mask("taylor@example.com", '*', 3, Some(10));
// Result: "tay**********e.com"
```

#### UUID/ULID Generation
```rust
// Generate UUID v4
let uuid = Str::uuid();

// Generate ULID
let ulid = Str::ulid(None);

// Custom factories for testing
Str::create_uuids_using(|| uuid::Uuid::nil());
```

#### Pluralization
```rust
// Basic pluralization
assert_eq!(Str::plural("car", 2, false), "cars");
assert_eq!(Str::plural("child", 2, false), "children");

// With count
assert_eq!(Str::plural("car", 2, true), "2 cars");

// Singularization
assert_eq!(Str::singular("cars"), "car");
assert_eq!(Str::singular("children"), "child");
```

#### Base64 Encoding
```rust
let encoded = Str::to_base64("Hello World");
let decoded = Str::from_base64(&encoded, false).unwrap();
```

## Configuration Options

### ExcerptOptions
```rust
use string_rs::ExcerptOptions;

let options = ExcerptOptions {
    radius: 100,
    omission: "...".to_string(),
};
```

### MarkdownOptions
```rust
use string_rs::MarkdownOptions;

let options = MarkdownOptions {
    tables: true,
    footnotes: true,
    strikethrough: true,
    tasklists: true,
};
```

## Testing

Run the test suite:

```bash
cargo test
```

All 47 tests cover the core functionality and edge cases.

## Dependencies

- `regex` - Regular expression support
- `unicode-segmentation` - Unicode-aware string segmentation
- `unidecode` - ASCII transliteration
- `base64` - Base64 encoding/decoding
- `uuid` - UUID generation
- `ulid` - ULID generation
- `rand` - Random number generation
- `once_cell` - Thread-safe static initialization
- `serde_json` - JSON validation
- `url` - URL validation
- `chrono` - Date/time handling
- `pulldown-cmark` - Markdown processing
- `textwrap` - Text wrapping

## Performance

The library uses caching for expensive operations like case conversions to improve performance in repeated operations. Caches can be cleared using:

```rust
Str::flush_cache();
```

## Unicode Support

Full Unicode support is provided through the `unicode-segmentation` crate, ensuring proper handling of:
- Grapheme clusters
- Multi-byte characters
- Emoji and symbols
- International text

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Changelog

### 0.1.0
- Initial release
- Core string manipulation functions
- Unicode support
- Markdown processing
- UUID/ULID generation
- Comprehensive test suite
