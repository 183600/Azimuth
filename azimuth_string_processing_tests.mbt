// Azimuth 字符串处理测试
// 专注于测试字符串操作和处理功能

// 测试1: 基本字符串操作
test "基本字符串操作测试" {
  let str1 = "hello"
  let str2 = "world"
  
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq(str1 + " " + str2, "hello world")
  
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  
  let single_char = "a"
  assert_eq(single_char.length(), 1)
}

// 测试2: 字符串反转
test "字符串反转测试" {
  assert_eq(string_reverse("hello"), "olleh")
  assert_eq(string_reverse("world"), "dlrow")
  assert_eq(string_reverse(""), "")
  assert_eq(string_reverse("a"), "a")
  assert_eq(string_reverse("ab"), "ba")
  assert_eq(string_reverse("racecar"), "racecar")  // 回文
}

fn string_reverse(s : String) -> String {
  let chars = s.to_array()
  let n = chars.length()
  let result = []
  
  for i = n - 1; i >= 0; i = i - 1 {
    result.push(chars[i])
  }
  
  result.from_array()
}

// 测试3: 字符串查找
test "字符串查找测试" {
  assert_eq(string_contains("hello world", "world"), true)
  assert_eq(string_contains("hello world", "hello"), true)
  assert_eq(string_contains("hello world", "lo wo"), true)
  assert_eq(string_contains("hello world", "xyz"), false)
  assert_eq(string_contains("hello world", ""), true)
  assert_eq(string_contains("", "anything"), false)
  assert_eq(string_contains("", ""), true)
}

fn string_contains(s : String, substr : String) -> Bool {
  let s_len = s.length()
  let sub_len = substr.length()
  
  if (sub_len == 0) {
    return true
  }
  
  if (sub_len > s_len) {
    return false
  }
  
  for i = 0; i <= s_len - sub_len; i = i + 1 {
    let match = true
    for j = 0; j < sub_len; j = j + 1 {
      if (s[i + j] != substr[j]) {
        match = false
        break
      }
    }
    if (match) {
      return true
    }
  }
  
  false
}

// 测试4: 字符串分割
test "字符串分割测试" {
  let result1 = string_split("a,b,c", ",")
  assert_eq(result1, ["a", "b", "c"])
  
  let result2 = string_split("apple,banana,cherry", ",")
  assert_eq(result2, ["apple", "banana", "cherry"])
  
  let result3 = string_split("hello", ",")
  assert_eq(result3, ["hello"])
  
  let result4 = string_split("", ",")
  assert_eq(result4, [""])
  
  let result5 = string_split("a,b,c,", ",")
  assert_eq(result5, ["a", "b", "c", ""])
}

fn string_split(s : String, delimiter : String) -> Array[String] {
  if (delimiter.length() == 0) {
    return [s]
  }
  
  let result = []
  let start = 0
  let s_len = s.length()
  let delim_len = delimiter.length()
  
  for i = 0; i <= s_len - delim_len; i = i + 1 {
    let match = true
    for j = 0; j < delim_len; j = j + 1 {
      if (s[i + j] != delimiter[j]) {
        match = false
        break
      }
    }
    
    if (match) {
      let substring = s.substring(start, i)
      result.push(substring)
      start = i + delim_len
      i = i + delim_len - 1  // 跳过分隔符
    }
  }
  
  let last_part = s.substring(start, s_len)
  result.push(last_part)
  
  result
}

// 测试5: 字符串连接
test "字符串连接测试" {
  let strings1 = ["a", "b", "c"]
  assert_eq(string_join(strings1, ","), "a,b,c")
  
  let strings2 = ["apple", "banana", "cherry"]
  assert_eq(string_join(strings2, ", "), "apple, banana, cherry")
  
  let strings3 = ["hello"]
  assert_eq(string_join(strings3, ","), "hello")
  
  let empty_array = []
  assert_eq(string_join(empty_array, ","), "")
  
  let strings4 = ["", "a", "", "b", ""]
  assert_eq(string_join(strings4, ","), ",a,,b,")
}

fn string_join(strings : Array[String], delimiter : String) -> String {
  if (strings.length() == 0) {
    return ""
  }
  
  let result = strings[0]
  
  for i = 1; i < strings.length(); i = i + 1 {
    result = result + delimiter + strings[i]
  }
  
  result
}

// 测试6: 字符串去除空格
test "字符串去除空格测试" {
  assert_eq(string_trim("  hello  "), "hello")
  assert_eq(string_trim("hello"), "hello")
  assert_eq(string_trim("   "), "")
  assert_eq(string_trim(""), "")
  assert_eq(string_trim("  hello world  "), "hello world")
  assert_eq(string_trim("\t hello \n"), "hello")  // 假设只处理空格
}

fn string_trim(s : String) -> String {
  let s_len = s.length()
  let start = 0
  let end = s_len - 1
  
  // 找到第一个非空格字符
  while (start <= end and s[start] == ' ') {
    start = start + 1
  }
  
  // 找到最后一个非空格字符
  while (end >= start and s[end] == ' ') {
    end = end - 1
  }
  
  if (start > end) {
    return ""
  }
  
  s.substring(start, end + 1)
}

// 测试7: 字符串转换
test "字符串转换测试" {
  assert_eq(string_to_upper("hello"), "HELLO")
  assert_eq(string_to_upper("WORLD"), "WORLD")
  assert_eq(string_to_upper("HeLLo WoRLd"), "HELLO WORLD")
  assert_eq(string_to_upper(""), "")
  
  assert_eq(string_to_lower("HELLO"), "hello")
  assert_eq(string_to_lower("world"), "world")
  assert_eq(string_to_lower("HeLLo WoRLd"), "hello world")
  assert_eq(string_to_lower(""), "")
}

fn string_to_upper(s : String) -> String {
  let result = []
  
  for i = 0; i < s.length(); i = i + 1 {
    let char = s[i]
    if ('a' <= char and char <= 'z') {
      result.push(char - 32)  // 转换为大写
    } else {
      result.push(char)
    }
  }
  
  result.from_array()
}

fn string_to_lower(s : String) -> String {
  let result = []
  
  for i = 0; i < s.length(); i = i + 1 {
    let char = s[i]
    if ('A' <= char and char <= 'Z') {
      result.push(char + 32)  // 转换为小写
    } else {
      result.push(char)
    }
  }
  
  result.from_array()
}

// 测试8: 字符串替换
test "字符串替换测试" {
  assert_eq(string_replace("hello world", "world", "universe"), "hello universe")
  assert_eq(string_replace("hello world hello", "hello", "hi"), "hi world hi")
  assert_eq(string_replace("hello", "xyz", "abc"), "hello")  // 没有匹配
  assert_eq(string_replace("", "a", "b"), "")
  assert_eq(string_replace("aaaa", "a", "b"), "bbbb")
  assert_eq(string_replace("abcabc", "abc", "x"), "xx")
}

fn string_replace(s : String, old_substr : String, new_substr : String) -> String {
  if (old_substr.length() == 0) {
    return s
  }
  
  let result = []
  let i = 0
  let s_len = s.length()
  let old_len = old_substr.length()
  
  while (i <= s_len - old_len) {
    let match = true
    for j = 0; j < old_len; j = j + 1 {
      if (s[i + j] != old_substr[j]) {
        match = false
        break
      }
    }
    
    if (match) {
      // 添加替换字符串
      for k = 0; k < new_substr.length(); k = k + 1 {
        result.push(new_substr[k])
      }
      i = i + old_len
    } else {
      result.push(s[i])
      i = i + 1
    }
  }
  
  // 添加剩余字符
  while (i < s_len) {
    result.push(s[i])
    i = i + 1
  }
  
  result.from_array()
}