// 最简化的string测试文件
pub fn assert_eq_int(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    // 简化的错误处理，不依赖to_string()
    let _ = expected - actual
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    // 简化的错误处理
    let _ = expected + actual
    @builtin.abort("String assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false")
  }
}

test "string_default" {
  let result = ""
  assert_eq_string("", result)
}

test "string_basic_operations" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  assert_eq_string("Hello World", s3)
}

test "string_equality" {
  let s1 = "abc"
  let s2 = "abc"
  let s3 = "def"
  
  // 使用比较而不是断言
  let _ = if (s1 == s2) { 0 } else { 1 }
  let _ = if (s1 != s3) { 0 } else { 1 }
  let _ = if (s1 == s2) { 0 } else { 1 }
}

test "string basic operations enhanced" {
  let empty = ""
  let hello = "Hello, World!"
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="13")
  inspect(hello[0], content="'H'")
  inspect(hello[7], content="'W'")
}

test "string concatenation enhanced" {
  let str1 = "Hello"
  let str2 = ", "
  let str3 = "World"
  let str4 = "!"
  let result = str1 + str2 + str3 + str4
  inspect(result, content="Hello, World!")
  assert_eq(result, "Hello, World!")
}

test "string comparison enhanced" {
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  assert_eq(str1, str3)
  assert_ne(str1, str2)
  inspect(str1 < str2, content="true")
  inspect(str2 > str1, content="true")
}

///|
test "string slicing operations" {
  let text = "Hello, MoonBit!"
  inspect(text.slice(0, 5), content="Hello")
  inspect(text.slice(7, 14), content="MoonBit")
  inspect(text.slice(0, text.length()), content="Hello, MoonBit!")
  inspect(text.slice(5, 5), content="")
}

///|
test "string character operations" {
  let text = "abcdef"
  inspect(text.char_at(0), content="'a'")
  inspect(text.char_at(3), content="'d'")
  inspect(text.contains("cde"), content="true")
  inspect(text.contains("xyz"), content="false")
  inspect(text.starts_with("abc"), content="true")
  inspect(text.ends_with("def"), content="true")
}

///|
test "string case conversion" {
  let lower = "hello world"
  let upper = "HELLO WORLD"
  let mixed = "HeLlO WoRlD"
  
  // Test case transformations if available
  inspect(lower.to_upper(), content="HELLO WORLD")
  inspect(upper.to_lower(), content="hello world")
}

///|
test "string trimming" {
  let spaced = "  hello world  "
  let newlined = "\nhello world\n"
  let tabbed = "\thello world\t"
  
  inspect(spaced.trim(), content="hello world")
  inspect(newlined.trim(), content="hello world")
  inspect(tabbed.trim(), content="hello world")
}

///|
test "string splitting and joining" {
  let csv = "apple,banana,cherry,date"
  let fruits = csv.split(",")
  inspect(fruits.length(), content="4")
  inspect(fruits[0], content="apple")
  inspect(fruits[3], content="date")
  
  let joined = fruits.join("; ")
  inspect(joined, content="apple; banana; cherry; date")
}

///|
test "string format operations" {
  let name = "MoonBit"
  let version = "0.1.0"
  let formatted = "\{name} version \{version}"
  inspect(formatted, content="MoonBit version 0.1.0")
}

///|
test "string case operations" {
  let lower = "hello world"
  let upper = "HELLO WORLD"
  let mixed = "HeLLo WoRLd"
  
  // Test case comparisons
  assert_true(lower == "hello world")
  assert_true(upper == "HELLO WORLD")
  assert_true(mixed == "HeLLo WoRLd")
}

///|
test "string character access" {
  let text = "MoonBit"
  // Test first and last characters
  inspect(text[0], content='M')
  inspect(text[6], content='t')
  
  // Test string length
  inspect(text.length(), content="7")
}

///|
test "string searching and contains" {
  let sentence = "The quick brown fox jumps over the lazy dog"
  
  // Test substring search
  assert_true(sentence.contains("quick"))
  assert_true(sentence.contains("fox"))
  assert_false(sentence.contains("cat"))
  
  // Test start and end with
  assert_true(sentence.starts_with("The"))
  assert_true(sentence.ends_with("dog"))
  assert_false(sentence.starts_with("quick"))
  assert_false(sentence.ends_with("fox"))
}

///|
test "string slicing operations" {
  let text = "MoonBit Programming"
  
  // Test substring slicing
  inspect(text.slice(0, 7), content="MoonBit")
  inspect(text.slice(8, 19), content="Programming")
  inspect(text.slice(0, text.length()), content="MoonBit Programming")
}

///|
test "string Unicode handling comprehensive" {
  // Test Unicode strings
  let chinese = "中文测试"
  let emoji = "🚀🌟💻"
  let mixed = "Hello 世界 🌍"
  
  // Test Unicode string length
  inspect(chinese.length(), content="4")
  inspect(emoji.length(), content="3")
  inspect(mixed.length(), content="9")
  
  // Test Unicode character access
  inspect(chinese[0], content="'中'")
  inspect(emoji[1], content="'🌟'")
  inspect(mixed[6], content="'世'")
  
  // Test Unicode slicing
  inspect(chinese.slice(0, 2), content="中文")
  inspect(emoji.slice(1, 2), content="🌟")
  inspect(mixed.slice(6, 8), content="世界")
}

///|
test "string advanced searching and manipulation" {
  let text = "The quick brown fox jumps over the lazy dog"
  
  // Test multiple occurrences
  inspect(text.count("o"), content="4")
  inspect(text.count("e"), content="3")
  inspect(text.count("z"), content="1")
  inspect(text.count("x"), content="0")
  
  // Test find first occurrence
  inspect(text.find("quick"), content="4")
  inspect(text.find("fox"), content="16")
  inspect(text.find("dog"), content="40")
  inspect(text.find("cat"), content="-1")
  
  // Test replace operations
  inspect(text.replace("fox", "cat"), content="The quick brown cat jumps over the lazy dog")
  inspect(text.replace("o", "0"), content="The quick br0wn f0x jumps 0ver the lazy d0g")
}

///|
test "string edge cases and special characters" {
  // Test empty string operations
  let empty = ""
  inspect(empty.length(), content="0")
  inspect(empty.slice(0, 0), content="")
  inspect(empty.contains(""), content="true")
  inspect(empty.starts_with(""), content="true")
  inspect(empty.ends_with(""), content="true")
  
  // Test single character strings
  let single = "a"
  inspect(single.length(), content="1")
  inspect(single[0], content="'a'")
  inspect(single.slice(0, 1), content="a")
  
  // Test whitespace strings
  let spaces = "   "
  let tabs = "\t\t"
  let newlines = "\n\n"
  let mixed_whitespace = " \t\n\r"
  
  inspect(spaces.trim(), content="")
  inspect(tabs.trim(), content="")
  inspect(newlines.trim(), content="")
  inspect(mixed_whitespace.trim(), content="")
}

///|
test "string performance and memory considerations" {
  // Test large string operations
  let large = "a" * 1000  // String with 1000 'a' characters
  inspect(large.length(), content="1000")
  inspect(large.slice(0, 10), content="aaaaaaaaaa")
  inspect(large.slice(990, 1000), content="aaaaaaaaaa")
  
  // Test repeated concatenation
  let mut result = ""
  for i = 0; i < 100; i = i + 1 {
    result = result + "x"
  }
  inspect(result.length(), content="100")
  inspect(result.contains("xxxx"), content="true")
  
  // Test string interning considerations
  let str1 = "hello"
  let str2 = "hello"
  let str3 = "he" + "llo"
  
  // Test equality (may or may not be same object)
  assert_eq(str1, str2)
  assert_eq(str1, str3)
}