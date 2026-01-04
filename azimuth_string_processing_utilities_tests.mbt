// Azimuth Telemetry System - String Processing Utilities Tests
// This file contains test cases for string processing utilities used in the telemetry system

// Test 1: String Tokenization
test "string tokenization" {
  let input_string = "token1,token2,token3,token4"
  let delimiter = ","
  
  // Test basic tokenization
  let tokens = StringUtil::tokenize(input_string, delimiter)
  assert_eq(tokens.length(), 4)
  assert_eq(tokens[0], "token1")
  assert_eq(tokens[1], "token2")
  assert_eq(tokens[2], "token3")
  assert_eq(tokens[3], "token4")
  
  // Test tokenization with different delimiter
  let space_delimited = "word1 word2 word3"
  let space_tokens = StringUtil::tokenize(space_delimited, " ")
  assert_eq(space_tokens.length(), 3)
  
  // Test tokenization with empty string
  let empty_string = ""
  let empty_tokens = StringUtil::tokenize(empty_string, ",")
  assert_eq(empty_tokens.length(), 0)
  
  // Test tokenization with delimiter not found
  let no_delimiter = "nodelimitershere"
  let no_delim_tokens = StringUtil::tokenize(no_delimiter, ",")
  assert_eq(no_delim_tokens.length(), 1)
  assert_eq(no_delim_tokens[0], "nodelimitershere")
}

// Test 2: String Validation
test "string validation" {
  // Test empty string validation
  assert_true(StringUtil::is_empty(""))
  assert_false(StringUtil::is_empty("not empty"))
  
  // Test whitespace validation
  assert_true(StringUtil::is_whitespace("   "))
  assert_true(StringUtil::is_whitespace("\t\n\r"))
  assert_false(StringUtil::is_whitespace(" content "))
  
  // Test alphanumeric validation
  assert_true(StringUtil::is_alphanumeric("abc123"))
  assert_false(StringUtil::is_alphanumeric("abc-123"))
  assert_false(StringUtil::is_alphanumeric("abc 123"))
  
  // Test numeric validation
  assert_true(StringUtil::is_numeric("12345"))
  assert_true(StringUtil::is_numeric("123.45"))
  assert_false(StringUtil::is_numeric("12a45"))
  assert_false(StringUtil::is_numeric("12 45"))
  
  // Test email validation
  assert_true(StringUtil::is_email("test@example.com"))
  assert_true(StringUtil::is_email("user.name@domain.co.uk"))
  assert_false(StringUtil::is_email("invalid-email"))
  assert_false(StringUtil::is_email("@domain.com"))
  assert_false(StringUtil::is_email("user@"))
  
  // Test URL validation
  assert_true(StringUtil::is_url("https://www.example.com"))
  assert_true(StringUtil::is_url("http://api.example.org/v1"))
  assert_false(StringUtil::is_url("not-a-url"))
  assert_false(StringUtil::is_url("www.example.com")) // Missing protocol
}

// Test 3: String Transformation
test "string transformation" {
  // Test case conversion
  assert_eq(StringUtil::to_uppercase("hello"), "HELLO")
  assert_eq(StringUtil::to_lowercase("WORLD"), "world")
  assert_eq(StringUtil::to_capitalize("hello world"), "Hello world")
  
  // Test trimming
  assert_eq(StringUtil::trim("  content  "), "content")
  assert_eq(StringUtil::trim_left("  content"), "content")
  assert_eq(StringUtil::trim_right("content  "), "content")
  assert_eq(StringUtil::trim("\t\ncontent\r\n"), "content")
  
  // Test padding
  assert_eq(StringUtil::pad_left("test", 8, "0"), "0000test")
  assert_eq(StringUtil::pad_right("test", 8, "0"), "test0000")
  assert_eq(StringUtil::pad_center("test", 9, "0"), "00test000")
  
  // Test reversing
  assert_eq(StringUtil::reverse("hello"), "olleh")
  assert_eq(StringUtil::reverse(""), "")
  assert_eq(StringUtil::reverse("a"), "a")
  
  // Test substring operations
  assert_eq(StringUtil::substring("hello world", 0, 5), "hello")
  assert_eq(StringUtil::substring("hello world", 6, 5), "world")
  assert_eq(StringUtil::substring("hello", 1, 3), "ell")
}

// Test 4: String Search and Replace
test "string search and replace" {
  let text = "The quick brown fox jumps over the lazy dog"
  
  // Test contains
  assert_true(StringUtil::contains(text, "quick"))
  assert_true(StringUtil::contains(text, "dog"))
  assert_false(StringUtil::contains(text, "cat"))
  
  // Test starts with
  assert_true(StringUtil::starts_with(text, "The"))
  assert_false(StringUtil::starts_with(text, "quick"))
  
  // Test ends with
  assert_true(StringUtil::ends_with(text, "dog"))
  assert_false(StringUtil::ends_with(text, "lazy"))
  
  // Test index of
  assert_eq(StringUtil::index_of(text, "quick"), 4)
  assert_eq(StringUtil::index_of(text, "fox"), 16)
  assert_eq(StringUtil::index_of(text, "cat"), -1)
  
  // Test last index of
  assert_eq(StringUtil::last_index_of("hello world hello", "hello"), 12)
  assert_eq(StringUtil::last_index_of("hello world", "world"), 6)
  assert_eq(StringUtil::last_index_of("hello world", "cat"), -1)
  
  // Test replace
  assert_eq(StringUtil::replace(text, "quick", "slow"), "The slow brown fox jumps over the lazy dog")
  assert_eq(StringUtil::replace(text, "dog", "cat"), "The quick brown fox jumps over the lazy cat")
  assert_eq(StringUtil::replace(text, "cat", "mouse"), text) // No replacement
  
  // Test replace all
  assert_eq(StringUtil::replace_all("hello hello hello", "hello", "hi"), "hi hi hi")
  assert_eq(StringUtil::replace_all("one two one two", "one", "uno"), "uno two uno two")
}

// Test 5: String Splitting and Joining
test "string splitting and joining" {
  // Test splitting
  let csv_data = "name,age,city,country"
  let csv_fields = StringUtil::split(csv_data, ",")
  assert_eq(csv_fields.length(), 4)
  assert_eq(csv_fields[0], "name")
  assert_eq(csv_fields[1], "age")
  assert_eq(csv_fields[2], "city")
  assert_eq(csv_fields[3], "country")
  
  // Test splitting with limit
  let limited_split = StringUtil::split_limit("a,b,c,d,e", ",", 3)
  assert_eq(limited_split.length(), 3)
  assert_eq(limited_split[0], "a")
  assert_eq(limited_split[1], "b")
  assert_eq(limited_split[2], "c,d,e")
  
  // Test joining
  let words = ["hello", "world", "test"]
  let joined = StringUtil::join(words, " ")
  assert_eq(joined, "hello world test")
  
  let comma_joined = StringUtil::join(words, ",")
  assert_eq(comma_joined, "hello,world,test")
  
  // Test joining empty array
  let empty_array = []
  let empty_joined = StringUtil::join(empty_array, ",")
  assert_eq(empty_joined, "")
}

// Test 6: String Encoding and Decoding
test "string encoding and decoding" {
  // Test base64 encoding
  let original = "Hello, World!"
  let encoded = StringUtil::base64_encode(original)
  assert_eq(encoded, "SGVsbG8sIFdvcmxkIQ==")
  
  // Test base64 decoding
  let decoded = StringUtil::base64_decode(encoded)
  match decoded {
    Some(value) => assert_eq(value, original)
    None => assert_true(false)
  }
  
  // Test URL encoding
  let url_text = "hello world!@#$%^&*()"
  let url_encoded = StringUtil::url_encode(url_text)
  assert_eq(url_encoded, "hello%20world%21%40%23%24%25%5E%26*%28%29")
  
  // Test URL decoding
  let url_decoded = StringUtil::url_decode(url_encoded)
  match url_decoded {
    Some(value) => assert_eq(value, url_text)
    None => assert_true(false)
  }
  
  // Test HTML encoding
  let html_text = "<script>alert('xss')</script>"
  let html_encoded = StringUtil::html_encode(html_text)
  assert_eq(html_encoded, "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;")
  
  // Test HTML decoding
  let html_decoded = StringUtil::html_decode(html_encoded)
  match html_decoded {
    Some(value) => assert_eq(value, html_text)
    None => assert_true(false)
  }
}

// Test 7: String Format and Parse
test "string format and parse" {
  // Test string formatting
  let formatted = StringUtil::format("Hello, {0}! Today is {1}.", ["Alice", "Monday"])
  assert_eq(formatted, "Hello, Alice! Today is Monday.")
  
  let number_formatted = StringUtil::format("Value: {0}, Count: {1}", ["42", "100"])
  assert_eq(number_formatted, "Value: 42, Count: 100")
  
  // Test parsing integers
  let int_result = StringUtil::parse_int("123")
  match int_result {
    Some(value) => assert_eq(value, 123)
    None => assert_true(false)
  }
  
  let invalid_int = StringUtil::parse_int("not_a_number")
  match invalid_int {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test parsing floats
  let float_result = StringUtil::parse_float("3.14")
  match float_result {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  let invalid_float = StringUtil::parse_float("not_a_float")
  match invalid_float {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test parsing booleans
  let bool_true = StringUtil::parse_bool("true")
  match bool_true {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let bool_false = StringUtil::parse_bool("false")
  match bool_false {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  let invalid_bool = StringUtil::parse_bool("maybe")
  match invalid_bool {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}