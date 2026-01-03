// Azimuth 数据处理和转换测试
// 专注于测试数据处理、转换和验证功能

// 测试1: 数据类型转换
test "数据类型转换测试" {
  // 整数到字符串转换
  let int_to_string = Int.to_string(42)
  assert_eq(int_to_string, "42")
  
  let negative_int_to_string = Int.to_string(-100)
  assert_eq(negative_int_to_string, "-100")
  
  // 字符串到整数转换
  let string_to_int = "123"
  match Int.parse(string_to_int) {
    Some(value) => assert_eq(value, 123)
    None => assert_true(false)
  }
  
  let negative_string_to_int = "-456"
  match Int.parse(negative_string_to_int) {
    Some(value) => assert_eq(value, -456)
    None => assert_true(false)
  }
  
  // 无效字符串转换
  let invalid_string = "abc"
  match Int.parse(invalid_string) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试2: 数据集合操作
test "数据集合操作测试" {
  // 列表创建和操作
  let empty_list = []
  assert_eq(empty_list.length(), 0)
  
  let number_list = [1, 2, 3, 4, 5]
  assert_eq(number_list.length(), 5)
  assert_eq(number_list[0], 1)
  assert_eq(number_list[4], 5)
  
  // 列表过滤
  let filtered_list = number_list.filter(fn(x) { x > 3 })
  assert_eq(filtered_list.length(), 2)
  assert_eq(filtered_list[0], 4)
  assert_eq(filtered_list[1], 5)
  
  // 列表映射
  let mapped_list = number_list.map(fn(x) { x * 2 })
  assert_eq(mapped_list.length(), 5)
  assert_eq(mapped_list[0], 2)
  assert_eq(mapped_list[4], 10)
  
  // 列表求和
  let sum = number_list.reduce(0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
}

// 测试3: 数据验证
test "数据验证测试" {
  // 邮箱格式验证
  let valid_emails = ["test@example.com", "user.name@domain.co.uk", "user+tag@example.org"]
  let invalid_emails = ["invalid-email", "@example.com", "user@", "user@.com"]
  
  for email in valid_emails {
    assert_true(validate_email(email))
  }
  
  for email in invalid_emails {
    assert_false(validate_email(email))
  }
  
  // 手机号码验证
  let valid_phone_numbers = ["1234567890", "+11234567890", "0123456789"]
  let invalid_phone_numbers = ["123", "abc1234567", "123456789012345"]
  
  for phone in valid_phone_numbers {
    assert_true(validate_phone_number(phone))
  }
  
  for phone in invalid_phone_numbers {
    assert_false(validate_phone_number(phone))
  }
}

// 测试4: 数据压缩和解压
test "数据压缩和解压测试" {
  // 简单字符串压缩
  let original_data = "This is a test string for compression testing"
  let compressed_data = compress_data(original_data)
  
  // 验证压缩后的数据长度小于原始数据
  assert_true(compressed_data.length() < original_data.length())
  
  // 解压并验证数据一致性
  let decompressed_data = decompress_data(compressed_data)
  assert_eq(decompressed_data, original_data)
  
  // 空字符串压缩测试
  let empty_string = ""
  let compressed_empty = compress_data(empty_string)
  let decompressed_empty = decompress_data(compressed_empty)
  assert_eq(decompressed_empty, empty_string)
  
  // 重复字符压缩测试
  let repetitive_data = "AAAAABBBBBCCCCCDDDDD"
  let compressed_repetitive = compress_data(repetitive_data)
  let decompressed_repetitive = decompress_data(compressed_repetitive)
  assert_eq(decompressed_repetitive, repetitive_data)
}

// 测试5: 数据格式转换
test "数据格式转换测试" {
  // JSON 格式转换
  let json_data = "{\"name\":\"John\",\"age\":30,\"city\":\"New York\"}"
  let parsed_data = parse_json(json_data)
  
  match parsed_data {
    JsonObject(obj) => {
      match obj["name"] {
        Some(JsonString(name)) => assert_eq(name, "John")
        _ => assert_true(false)
      }
      match obj["age"] {
        Some(JsonNumber(age)) => assert_eq(age, 30.0)
        _ => assert_true(false)
      }
      match obj["city"] {
        Some(JsonString(city)) => assert_eq(city, "New York")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // XML 格式转换
  let xml_data = "<person><name>John</name><age>30</age><city>New York</city></person>"
  let parsed_xml = parse_xml(xml_data)
  
  match parsed_xml {
    XmlElement("person", _, children) => {
      assert_eq(children.length(), 3)
      match children[0] {
        XmlElement("name", _, [XmlText(name)]) => assert_eq(name, "John")
        _ => assert_true(false)
      }
      match children[1] {
        XmlElement("age", _, [XmlText(age)]) => assert_eq(age, "30")
        _ => assert_true(false)
      }
      match children[2] {
        XmlElement("city", _, [XmlText(city)]) => assert_eq(city, "New York")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 辅助函数：邮箱验证
fn validate_email(email : String) -> Bool {
  // 简化的邮箱验证逻辑
  let has_at = email.contains("@")
  let has_dot_after_at = {
    match email.index_of("@") {
      Some(at_index) => {
        let domain_part = email.substring(at_index + 1, email.length() - at_index - 1)
        domain_part.contains(".")
      }
      None => false
    }
  }
  has_at && has_dot_after_at
}

// 辅助函数：手机号码验证
fn validate_phone_number(phone : String) -> Bool {
  // 简化的手机号码验证逻辑
  let digits_only = phone.filter(fn(c) { c >= '0' && c <= '9' })
  digits_only.length() >= 10 && digits_only.length() <= 15
}

// 辅助函数：数据压缩
fn compress_data(data : String) -> String {
  // 简化的压缩逻辑（实际项目中应使用专业压缩算法）
  let compressed = []
  let i = 0
  while i < data.length() {
    let current_char = data[i]
    let count = 1
    
    // 计算连续相同字符的数量
    while i + count < data.length() && data[i + count] == current_char {
      count = count + 1
    }
    
    // 如果字符重复次数大于等于3，使用压缩格式
    if count >= 3 {
      compressed.push('{')
      compressed.push(current_char)
      compressed.push(Int.to_string(count))
      compressed.push('}')
    } else {
      // 否则直接添加字符
      let j = 0
      while j < count {
        compressed.push(current_char)
        j = j + 1
      }
    }
    
    i = i + count
  }
  
  String.from_array(compressed)
}

// 辅助函数：数据解压
fn decompress_data(compressed : String) -> String {
  // 简化的解压逻辑
  let decompressed = []
  let i = 0
  
  while i < compressed.length() {
    if compressed[i] == '{' {
      // 压缩格式：{字符X次数}
      if i + 2 < compressed.length() {
        let char_to_repeat = compressed[i + 1]
        let count_str = ""
        let j = i + 2
        
        // 提取次数
        while j < compressed.length() && compressed[j] != '}' {
          count_str = count_str + compressed[j]
          j = j + 1
        }
        
        match Int.parse(count_str) {
          Some(count) => {
            let k = 0
            while k < count {
              decompressed.push(char_to_repeat)
              k = k + 1
            }
          }
          None => {
            // 如果解析失败，直接添加原始字符
            decompressed.push(compressed[i])
          }
        }
        
        i = j + 1
      } else {
        decompressed.push(compressed[i])
        i = i + 1
      }
    } else {
      // 普通字符，直接添加
      decompressed.push(compressed[i])
      i = i + 1
    }
  }
  
  String.from_array(decompressed)
}

// JSON 数据类型定义
type JsonValue {
  JsonString(String)
  JsonNumber(Float)
  JsonBoolean(Bool)
  JsonArray(Array[JsonValue])
  JsonObject(Array[(String, JsonValue)])
  JsonNull
}

// 辅助函数：解析 JSON
fn parse_json(json : String) -> JsonValue {
  // 简化的 JSON 解析逻辑
  if json.length() > 0 && json[0] == '{' && json[json.length() - 1] == '}' {
    let inner = json.substring(1, json.length() - 2)
    let pairs = split_json_pairs(inner)
    let obj = []
    
    for pair in pairs {
      match split_key_value(pair) {
        Some((key, value)) => {
          obj.push((key, parse_json_value(value)))
        }
        None => {}
      }
    }
    
    JsonObject(obj)
  } else {
    JsonNull
  }
}

// 辅助函数：解析 JSON 值
fn parse_json_value(value : String) -> JsonValue {
  let trimmed = value.trim()
  
  if trimmed.length() > 0 && trimmed[0] == '"' && trimmed[trimmed.length() - 1] == '"' {
    // 字符串值
    JsonString(trimmed.substring(1, trimmed.length() - 2))
  } else if trimmed == "true" {
    JsonBoolean(true)
  } else if trimmed == "false" {
    JsonBoolean(false)
  } else if trimmed == "null" {
    JsonNull
  } else {
    // 尝试解析为数字
    match Float.parse(trimmed) {
      Some(num) => JsonNumber(num)
      None => JsonString(trimmed)
    }
  }
}

// 辅助函数：分割 JSON 键值对
fn split_json_pairs(pairs_str : String) -> Array[String] {
  let pairs = []
  let current = ""
  let in_string = false
  let i = 0
  
  while i < pairs_str.length() {
    let c = pairs_str[i]
    
    if c == '"' && (i == 0 || pairs_str[i - 1] != '\\') {
      in_string = !in_string
    }
    
    if c == ',' && !in_string {
      pairs.push(current.trim())
      current = ""
    } else {
      current = current + c
    }
    
    i = i + 1
  }
  
  if current.length() > 0 {
    pairs.push(current.trim())
  }
  
  pairs
}

// 辅助函数：分割键和值
fn split_key_value(pair : String) -> Option[(String, String)] {
  match pair.index_of(":") {
    Some(colon_index) => {
      let key = pair.substring(0, colon_index).trim()
      let value = pair.substring(colon_index + 1, pair.length() - colon_index - 1).trim()
      Some((key, value))
    }
    None => None
  }
}

// XML 数据类型定义
type XmlElement {
  XmlElement(String, Array[(String, String)], Array[XmlContent])
}

type XmlContent {
  XmlElement(XmlElement)
  XmlText(String)
  XmlComment(String)
}

// 辅助函数：解析 XML
fn parse_xml(xml : String) -> XmlElement {
  // 简化的 XML 解析逻辑
  if xml.length() > 0 && xml[0] == '<' && xml[xml.length() - 1] == '>' {
    match find_tag_name(xml) {
      Some(tag_name) => {
        let inner_content = extract_inner_content(xml, tag_name)
        let children = parse_xml_content(inner_content)
        XmlElement(tag_name, [], children)
      }
      None => XmlElement("root", [], [])
    }
  } else {
    XmlElement("root", [], [])
  }
}

// 辅助函数：查找标签名
fn find_tag_name(xml : String) -> Option[String] {
  if xml.length() > 1 && xml[0] == '<' {
    let i = 1
    let tag_name = ""
    
    while i < xml.length() && xml[i] != ' ' && xml[i] != '>' {
      tag_name = tag_name + xml[i]
      i = i + 1
    }
    
    Some(tag_name)
  } else {
    None
  }
}

// 辅助函数：提取内部内容
fn extract_inner_content(xml : String, tag_name : String) -> String {
  let start_tag = "<" + tag_name + ">"
  let end_tag = "</" + tag_name + ">"
  
  match xml.index_of(start_tag) {
    Some(start_index) => {
      let content_start = start_index + start_tag.length()
      match xml.index_of(end_tag) {
        Some(end_index) => {
          xml.substring(content_start, end_index - content_start)
        }
        None => ""
      }
    }
    None => ""
  }
}

// 辅助函数：解析 XML 内容
fn parse_xml_content(content : String) -> Array[XmlContent] {
  let children = []
  let i = 0
  
  while i < content.length() {
    if content[i] == '<' {
      // 处理元素
      match find_element_end(content, i) {
        Some(end_index) => {
          let element_str = content.substring(i, end_index - i)
          let element = parse_xml(element_str)
          children.push(XmlElement(element))
          i = end_index
        }
        None => {
          i = i + 1
        }
      }
    } else {
      // 处理文本
      let text_start = i
      while i < content.length() && content[i] != '<' {
        i = i + 1
      }
      let text = content.substring(text_start, i - text_start).trim()
      if text.length() > 0 {
        children.push(XmlText(text))
      }
    }
  }
  
  children
}

// 辅助函数：查找元素结束位置
fn find_element_end(content : String, start : Int) -> Option[Int] {
  if content[start] != '<' {
    None
  } else {
    match find_tag_name(content.substring(start, content.length() - start)) {
      Some(tag_name) => {
        let end_tag = "</" + tag_name + ">"
        match content.index_of(end_tag) {
          Some(end_index) => Some(end_index + end_tag.length())
          None => None
        }
      }
      None => None
    }
  }
}