// Azimuth 高级字符串处理测试用例
// 专注于字符串操作、文本处理和编码转换功能测试

// 测试1: 字符串基础操作扩展
test "字符串基础操作扩展测试" {
  // 测试字符串连接和重复
  let str1 = "Hello"
  let str2 = "World"
  assert_eq(str1 + " " + str2, "Hello World")
  assert_eq(str1 * 3, "HelloHelloHello")
  
  // 测试字符串长度和字符计数
  let unicode_str = "你好世界"
  assert_eq(unicode_str.length(), 4) // 字符数
  assert_eq(unicode_str.byte_length(), 12) // UTF-8字节数
  
  // 测试字符串大小写转换
  let mixed_case = "HeLLo WoRLd"
  assert_eq(mixed_case.to_uppercase(), "HELLO WORLD")
  assert_eq(mixed_case.to_lowercase(), "hello world")
  assert_eq(mixed_case.to_capitalized(), "Hello world")
  
  // 测试字符串比较
  assert_true("apple" < "banana")
  assert_true("Apple".compare_ignore_case("apple") == 0)
  assert_false("Apple".compare_ignore_case("Banana") == 0)
}

// 测试2: 字符串搜索和替换
test "字符串搜索和替换测试" {
  let text = "The quick brown fox jumps over the lazy dog"
  
  // 测试字符串包含检查
  assert_true(text.contains("quick"))
  assert_true(text.contains("lazy"))
  assert_false(text.contains("cat"))
  
  // 测试字符串位置查找
  assert_eq(text.find("quick"), Some(4))
  assert_eq(text.find("lazy"), Some(35))
  assert_eq(text.find("cat"), None)
  
  // 测试从后向前查找
  assert_eq(text.rfind("the"), Some(31)) // 第二个"the"
  assert_eq(text.rfind("fox"), Some(16))
  
  // 测试字符串替换
  let replaced1 = text.replace("fox", "cat")
  assert_eq(replaced1, "The quick brown cat jumps over the lazy dog")
  
  let replaced2 = text.replace("the", "a") // 只替换第一个
  assert_eq(replaced2, "The quick brown fox jumps over a lazy dog")
  
  let replaced_all = text.replace_all("the", "a") // 替换所有
  assert_eq(replaced_all, "The quick brown fox jumps over a lazy dog")
}

// 测试3: 字符串分割和连接
test "字符串分割和连接测试" {
  // 测试基本分割
  let csv_data = "apple,banana,cherry,date"
  let fruits = csv_data.split(",")
  assert_eq(fruits.length(), 4)
  assert_eq(fruits[0], "apple")
  assert_eq(fruits[3], "date")
  
  // 测试带空格的分割
  let sentence = "This is a test sentence"
  let words = sentence.split(" ")
  assert_eq(words.length(), 5)
  assert_eq(words[0], "This")
  assert_eq(words[4], "sentence")
  
  // 测试限制分割次数
  let limited = csv_data.split(",", 2)
  assert_eq(limited.length(), 2)
  assert_eq(limited[0], "apple")
  assert_eq(limited[1], "banana,cherry,date")
  
  // 测试数组连接
  let joined = fruits.join("; ")
  assert_eq(joined, "apple; banana; cherry; date")
  
  // 测试带前缀和后缀的连接
  let prefixed = fruits.join(", ", "[", "]")
  assert_eq(prefixed, "[apple, banana, cherry, date]")
}

// 测试4: 字符串修剪和填充
test "字符串修剪和填充测试" {
  // 测试空白字符修剪
  let padded_text = "   Hello World   "
  assert_eq(padded_text.trim(), "Hello World")
  assert_eq(padded_text.trim_left(), "Hello World   ")
  assert_eq(padded_text.trim_right(), "   Hello World")
  
  // 测试指定字符修剪
  let bracketed_text = "***Hello World***"
  assert_eq(bracketed_text.trim("*"), "Hello World")
  assert_eq(bracketed_text.trim_left("*"), "Hello World***")
  assert_eq(bracketed_text.trim_right("*"), "***Hello World")
  
  // 测试字符串填充
  let short_text = "Test"
  let padded_left = short_text.pad_left(10, ' ')
  assert_eq(padded_left, "      Test")
  
  let padded_right = short_text.pad_right(10, '-')
  assert_eq(padded_right, "Test------")
  
  let padded_center = short_text.pad_center(10, '_')
  assert_eq(padded_center, "___Test___")
}

// 测试5: 字符串子串操作
test "字符串子串操作测试" {
  let text = "Programming in MoonBit"
  
  // 测试子串提取
  assert_eq(text.substring(0, 11), "Programming")
  assert_eq(text.substring(12, 14), "in")
  assert_eq(text.substring(15, 22), "MoonBit")
  
  // 测试从位置到末尾的子串
  assert_eq(text.substring_from(12), "in MoonBit")
  assert_eq(text.substring_from(15), "MoonBit")
  
  // 测试到位置的子串
  assert_eq(text.substring_to(11), "Programming")
  assert_eq(text.substring_to(14), "Programming in")
  
  // 测试前缀和后缀检查
  assert_true(text.starts_with("Programming"))
  assert_true(text.ends_with("MoonBit"))
  assert_false(text.starts_with("MoonBit"))
  assert_false(text.ends_with("Programming"))
  
  // 测试获取前N个字符和后N个字符
  assert_eq(text.first_chars(5), "Progr")
  assert_eq(text.last_chars(7), "MoonBit")
}

// 测试6: 字符串模式匹配
test "字符串模式匹配测试" {
  // 测试通配符匹配
  assert_true("test.txt".matches("*.txt"))
  assert_true("document.pdf".matches("*.pdf"))
  assert_false("image.jpg".matches("*.txt"))
  
  assert_true("user_123".matches("user_*"))
  assert_true("admin_456".matches("*_456"))
  assert_false("guest789".matches("user_*"))
  
  // 测试正则表达式匹配（简化版）
  let email = "user@example.com"
  assert_true(email.matches_email_pattern())
  
  let phone = "123-456-7890"
  assert_true(phone.matches_phone_pattern())
  
  let invalid_email = "invalid.email"
  assert_false(invalid_email.matches_email_pattern())
  
  // 测试数字字符串验证
  assert_true("12345".is_numeric())
  assert_true("-123.45".is_numeric())
  assert_false("12a34".is_numeric())
  assert_false("abc".is_numeric())
  
  // 测试字母字符串验证
  assert_true("Hello".is_alpha())
  assert_true("World".is_alpha())
  assert_false("Hello123".is_alpha())
  assert_false("Hello World".is_alpha())
}

// 测试7: 字符串编码转换
test "字符串编码转换测试" {
  // 测试Base64编码和解码
  let original_text = "Hello, MoonBit!"
  let encoded = original_text.to_base64()
  assert_eq(encoded, "SGVsbG8sIE1vb25CaXQh")
  
  let decoded = encoded.from_base64()
  assert_eq(decoded, original_text)
  
  // 测试URL编码和解码
  let url_text = "Hello World! @#$%"
  let url_encoded = url_text.url_encode()
  assert_eq(url_encoded, "Hello%20World%21%20%40%23%24%25")
  
  let url_decoded = url_encoded.url_decode()
  assert_eq(url_decoded, url_text)
  
  // 测试HTML实体编码和解码
  let html_text = "<script>alert('Hello');</script>"
  let html_encoded = html_text.html_encode()
  assert_eq(html_encoded, "&lt;script&gt;alert(&#39;Hello&#39;);&lt;/script&gt;")
  
  let html_decoded = html_encoded.html_decode()
  assert_eq(html_decoded, html_text)
  
  // 测试Unicode转义序列
  let unicode_text = "你好世界"
  let unicode_escaped = unicode_text.to_unicode_escape()
  assert_eq(unicode_escaped, "\\u4F60\\u597D\\u4E16\\u754C")
  
  let unicode_unescaped = unicode_escaped.from_unicode_escape()
  assert_eq(unicode_unescaped, unicode_text)
}

// 测试8: 字符串格式化和模板
test "字符串格式化和模板测试" {
  // 测试字符串插值
  let name = "Alice"
  let age = 30
  let formatted = "Name: ${name}, Age: ${age}"
  assert_eq(formatted, "Name: Alice, Age: 30")
  
  // 测试数字格式化
  let price = 1234.5678
  let formatted_price = format_currency(price, "USD", 2)
  assert_eq(formatted_price, "$1,234.57")
  
  let percentage = 0.7543
  let formatted_percentage = format_percentage(percentage, 1)
  assert_eq(formatted_percentage, "75.4%")
  
  // 测试日期格式化
  let year = 2023
  let month = 12
  let day = 25
  let formatted_date = format_date(year, month, day, "YYYY-MM-DD")
  assert_eq(formatted_date, "2023-12-25")
  
  // 测试对齐格式化
  let items = ["Apple", "Banana", "Cherry"]
  let aligned = format_table(items, 10, "left")
  assert_eq(aligned, "Apple     \nBanana    \nCherry    ")
  
  let aligned_right = format_table(items, 10, "right")
  assert_eq(aligned_right, "     Apple\n    Banana\n    Cherry")
}

// 测试9: 字符串统计和分析
test "字符串统计和分析测试" {
  let text = "The quick brown fox jumps over the lazy dog. The dog was lazy."
  
  // 测试字符频率统计
  let char_freq = text.char_frequency()
  assert_eq(char_freq.get('T'), 2) // 大写T
  assert_eq(char_freq.get('h'), 3) // 小写h
  assert_eq(char_freq.get(' '), 9) // 空格
  
  // 测试单词频率统计
  let word_freq = text.word_frequency()
  assert_eq(word_freq.get("the"), 3) // 不区分大小写
  assert_eq(word_freq.get("lazy"), 2)
  assert_eq(word_freq.get("dog"), 2)
  
  // 测试字符串统计信息
  let stats = text.get_statistics()
  assert_eq(stats.char_count, 44) // 总字符数
  assert_eq(stats.word_count, 9) // 单词数
  assert_eq(stats.line_count, 1) // 行数
  assert_eq(stats.sentence_count, 2) // 句子数
  
  // 测试可读性指标
  let readability = text.get_readability_score()
  assert_true(readability.flesch_score > 0.0 && readability.flesch_score < 100.0)
  assert_true(readability.grade_level >= 1.0 && readability.grade_level <= 20.0)
}

// 测试10: 字符串转换和验证
test "字符串转换和验证测试" {
  // 测试字符串到数值转换
  assert_eq("42".to_int(), Some(42))
  assert_eq("-123".to_int(), Some(-123))
  assert_eq("3.14".to_int(), None)
  assert_eq("abc".to_int(), None)
  
  assert_eq("3.14".to_float(), Some(3.14))
  assert_eq("-2.5".to_float(), Some(-2.5))
  assert_eq("abc".to_float(), None)
  
  // 测试布尔值转换
  assert_eq("true".to_bool(), Some(true))
  assert_eq("false".to_bool(), Some(false))
  assert_eq("1".to_bool(), Some(true))
  assert_eq("0".to_bool(), Some(false))
  assert_eq("abc".to_bool(), None)
  
  // 测试字符串验证
  assert_true("john.doe@example.com".is_valid_email())
  assert_false("invalid.email".is_valid_email())
  
  assert_true("https://www.example.com".is_valid_url())
  assert_false("not.a.url".is_valid_url())
  
  assert_true("192.168.1.1".is_valid_ip())
  assert_false("999.999.999.999".is_valid_ip())
  
  assert_true("123-456-7890".is_valid_phone())
  assert_false("12-34-56".is_valid_phone())
}

// 辅助函数定义（实际实现中这些函数应该来自标准库或实现模块）
fn String::matches_email_pattern(self : String) -> Bool {
  self.contains("@") && self.contains(".") && self.index_of("@") < self.length() - 1
}

fn String::matches_phone_pattern(self : String) -> Bool {
  self.matches("\\d{3}-\\d{3}-\\d{4}")
}

fn String::is_numeric(self : String) -> Bool {
  let mut has_digit = false
  let mut has_dot = false
  let mut has_sign = false
  
  for i in 0..self.length() {
    let char = self[i]
    if char >= '0' && char <= '9' {
      has_digit = true
    } else if (char == '.' || char == ',') && !has_dot {
      has_dot = true
    } else if ((char == '-' || char == '+') && i == 0) {
      has_sign = true
    } else {
      return false
    }
  }
  
  has_digit
}

fn String::is_alpha(self : String) -> Bool {
  for i in 0..self.length() {
    let char = self[i]
    if !((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')) {
      return false
    }
  }
  true
}

fn String::to_base64(self : String) -> String {
  // 简化的Base64编码实现
  let result = ""
  // 实际实现需要进行Base64编码
  result
}

fn String::from_base64(self : String) -> String {
  // 简化的Base64解码实现
  let result = ""
  // 实际实现需要进行Base64解码
  result
}

fn String::url_encode(self : String) -> String {
  // 简化的URL编码实现
  let result = ""
  // 实际实现需要进行URL编码
  result
}

fn String::url_decode(self : String) -> String {
  // 简化的URL解码实现
  let result = ""
  // 实际实现需要进行URL解码
  result
}

fn String::html_encode(self : String) -> String {
  self.replace("<", "&lt;").replace(">", "&gt;").replace("'", "&#39;").replace("\"", "&quot;")
}

fn String::html_decode(self : String) -> String {
  self.replace("&lt;", "<").replace("&gt;", ">").replace("&#39;", "'").replace("&quot;", "\"")
}

fn String::to_unicode_escape(self : String) -> String {
  // 简化的Unicode转义实现
  let result = ""
  // 实际实现需要进行Unicode转义
  result
}

fn String::from_unicode_escape(self : String) -> String {
  // 简化的Unicode转义解码实现
  let result = ""
  // 实际实现需要进行Unicode转义解码
  result
}

fn format_currency(value : Float, currency : String, precision : Int) -> String {
  // 简化的货币格式化实现
  currency + value.to_string()
}

fn format_percentage(value : Float, precision : Int) -> String {
  (value * 100.0).to_string() + "%"
}

fn format_date(year : Int, month : Int, day : Int, format : String) -> String {
  // 简化的日期格式化实现
  year.to_string() + "-" + 
  (if month < 10 { "0" } else { "" }) + month.to_string() + "-" +
  (if day < 10 { "0" } else { "" }) + day.to_string()
}

fn format_table(items : Array[String], width : Int, alignment : String) -> String {
  // 简化的表格格式化实现
  let result = ""
  for item in items {
    let padded = if alignment == "left" {
      item.pad_right(width, ' ')
    } else {
      item.pad_left(width, ' ')
    }
    result = result + padded + "\n"
  }
  result
}

fn String::char_frequency(self : String) -> Map[Char, Int] {
  let freq = Map::empty()
  for i in 0..self.length() {
    let char = self[i]
    let count = freq.get_with_default(char, 0)
    freq.set(char, count + 1)
  }
  freq
}

fn String::word_frequency(self : String) -> Map[String, Int] {
  let freq = Map::empty()
  let words = self.split(" ")
  for word in words {
    let clean_word = word.to_lowercase()
    let count = freq.get_with_default(clean_word, 0)
    freq.set(clean_word, count + 1)
  }
  freq
}

type StringStatistics {
  char_count : Int
  word_count : Int
  line_count : Int
  sentence_count : Int
}

fn String::get_statistics(self : String) -> StringStatistics {
  let char_count = self.length()
  let word_count = self.split(" ").length()
  let line_count = self.split("\n").length()
  let sentence_count = self.split(".").length()
  
  { char_count, word_count, line_count, sentence_count }
}

type ReadabilityScore {
  flesch_score : Float
  grade_level : Float
}

fn String::get_readability_score(self : String) -> ReadabilityScore {
  // 简化的可读性评分实现
  { flesch_score: 65.5, grade_level: 8.5 }
}

fn String::to_int(self : String) -> Option[Int] {
  // 简化的字符串到整数转换
  try {
    Some(self.to_int())
  } catch {
    None
  }
}

fn String::to_float(self : String) -> Option[Float] {
  // 简化的字符串到浮点数转换
  try {
    Some(self.to_float())
  } catch {
    None
  }
}

fn String::to_bool(self : String) -> Option[Bool] {
  if self == "true" || self == "1" {
    Some(true)
  } else if self == "false" || self == "0" {
    Some(false)
  } else {
    None
  }
}

fn String::is_valid_email(self : String) -> Bool {
  self.contains("@") && self.contains(".") && self.index_of("@") > 0 && self.index_of("@") < self.length() - 1
}

fn String::is_valid_url(self : String) -> Bool {
  self.starts_with("http://") || self.starts_with("https://")
}

fn String::is_valid_ip(self : String) -> Bool {
  let parts = self.split(".")
  if parts.length() != 4 { return false }
  
  for part in parts {
    match part.to_int() {
      Some(num) => {
        if num < 0 || num > 255 { return false }
      }
      None => { return false }
    }
  }
  true
}

fn String::is_valid_phone(self : String) -> Bool {
  self.matches("\\d{3}-\\d{3}-\\d{4}")
}