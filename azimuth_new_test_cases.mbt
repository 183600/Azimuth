// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含实用的测试场景

test "string_length_operations" {
  // 测试字符串长度操作
  let str1 = "Hello"
  let str2 = "MoonBit"
  let str3 = ""
  let str4 = "测试字符串"
  
  // 计算字符串长度
  let len1 = @azimuth.string_length(str1)
  let len2 = @azimuth.string_length(str2)
  let len3 = @azimuth.string_length(str3)
  let len4 = @azimuth.string_length(str4)
  
  // 验证长度
  @azimuth.assert_eq(5, len1)
  @azimuth.assert_eq(7, len2)
  @azimuth.assert_eq(0, len3)
  @azimuth.assert_eq(5, len4)
}

test "array_sort_operations" {
  // 测试数组排序操作
  let unsorted = @azimuth.create_array_from_list([5, 2, 8, 1, 9])
  let sorted = @azimuth.sort_array(unsorted)
  
  // 验证排序结果
  @azimuth.assert_eq(1, @azimuth.array_get(sorted, 0))
  @azimuth.assert_eq(2, @azimuth.array_get(sorted, 1))
  @azimuth.assert_eq(5, @azimuth.array_get(sorted, 2))
  @azimuth.assert_eq(8, @azimuth.array_get(sorted, 3))
  @azimuth.assert_eq(9, @azimuth.array_get(sorted, 4))
}

test "conditional_logic" {
  // 测试条件逻辑
  let age = 18
  let is_student = true
  let score = 85
  
  // 测试年龄判断
  let is_adult = @azimuth.greater_or_equal(age, 18)
  @azimuth.assert_eq(true, is_adult)
  
  // 测试学生折扣
  let discount = @azimuth.if_then_else(is_student, 20, 0)
  @azimuth.assert_eq(20, discount)
  
  // 测试成绩等级
  let grade = @azimuth.if_then_else(
    @azimuth.greater_or_equal(score, 90),
    "A",
    @azimuth.if_then_else(
      @azimuth.greater_or_equal(score, 80),
      "B",
      "C"
    )
  )
  @azimuth.assert_eq_string("B", grade)
}

test "list_operations" {
  // 测试列表操作
  let list1 = @azimuth.create_list([1, 2, 3])
  let list2 = @azimuth.create_list([4, 5, 6])
  let combined = @azimuth.concat_lists(list1, list2)
  
  // 验证列表长度
  @azimuth.assert_eq(6, @azimuth.list_length(combined))
  
  // 验证列表元素
  @azimuth.assert_eq(1, @azimuth.list_get(combined, 0))
  @azimuth.assert_eq(3, @azimuth.list_get(combined, 2))
  @azimuth.assert_eq(6, @azimuth.list_get(combined, 5))
}

test "hash_map_operations" {
  // 测试哈希映射操作
  let map = @azimuth.create_hash_map()
  let map_with_key = @azimuth.hash_map_insert(map, "name", "Azimuth")
  let map_with_value = @azimuth.hash_map_insert(map_with_key, "version", "1.0")
  
  // 验证键值对
  @azimuth.assert_eq(true, @azimuth.hash_map_contains(map_with_value, "name"))
  @azimuth.assert_eq(true, @azimuth.hash_map_contains(map_with_value, "version"))
  @azimuth.assert_eq(false, @azimuth.hash_map_contains(map_with_value, "author"))
  
  // 验证值
  @azimuth.assert_eq_string("Azimuth", @azimuth.hash_map_get(map_with_value, "name"))
  @azimuth.assert_eq_string("1.0", @azimuth.hash_map_get(map_with_value, "version"))
}

test "file_operations" {
  // 测试文件操作
  let filename = "test.txt"
  let content = "Hello, MoonBit!"
  
  // 写入文件
  let write_result = @azimuth.write_file(filename, content)
  @azimuth.assert_eq(true, @azimuth.is_success(write_result))
  
  // 读取文件
  let read_result = @azimuth.read_file(filename)
  @azimuth.assert_eq(true, @azimuth.is_success(read_result))
  
  // 验证内容
  let file_content = @azimuth.get_value(read_result)
  @azimuth.assert_eq_string(content, file_content)
}

test "regex_operations" {
  // 测试正则表达式操作
  let email = "user@example.com"
  let phone = "123-456-7890"
  let invalid = "not-an-email"
  
  // 验证邮箱格式
  let email_pattern = @azimuth.compile_regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  @azimuth.assert_eq(true, @azimuth.regex_match(email_pattern, email))
  @azimuth.assert_eq(false, @azimuth.regex_match(email_pattern, invalid))
  
  // 验证电话格式
  let phone_pattern = @azimuth.compile_regex("^\\d{3}-\\d{3}-\\d{4}$")
  @azimuth.assert_eq(true, @azimuth.regex_match(phone_pattern, phone))
  @azimuth.assert_eq(false, @azimuth.regex_match(phone_pattern, email))
}

test "json_operations" {
  // 测试 JSON 操作
  let json_str = "{\"name\":\"Azimuth\",\"version\":\"1.0\",\"active\":true}"
  let parsed = @azimuth.parse_json(json_str)
  
  // 验证解析成功
  @azimuth.assert_eq(true, @azimuth.is_success(parsed))
  
  // 获取 JSON 对象
  let json_obj = @azimuth.get_value(parsed)
  
  // 验证字段值
  @azimuth.assert_eq_string("Azimuth", @azimuth.json_get_string(json_obj, "name"))
  @azimuth.assert_eq_string("1.0", @azimuth.json_get_string(json_obj, "version"))
  @azimuth.assert_eq(true, @azimuth.json_get_bool(json_obj, "active"))
}

test "time_operations" {
  // 测试时间操作
  let now = @azimuth.get_current_time()
  let tomorrow = @azimuth.add_days(now, 1)
  let next_week = @azimuth.add_days(now, 7)
  
  // 验证时间差
  let diff_tomorrow = @azimuth.days_between(now, tomorrow)
  let diff_week = @azimuth.days_between(now, next_week)
  
  @azimuth.assert_eq(1, diff_tomorrow)
  @azimuth.assert_eq(7, diff_week)
  
  // 验证格式化
  let formatted = @azimuth.format_time(now, "YYYY-MM-DD")
  @azimuth.assert_eq(true, @azimuth.string_length(formatted) > 0)
}

test "error_recovery" {
  // 测试错误恢复
  let safe_divide = fn(x : Int, y : Int) {
    @azimuth.if_then_else(
      @azimuth.equal(y, 0),
      @azimuth.create_error("Division by zero"),
      @azimuth.create_success(@azimuth.divide(x, y))
    )
  }
  
  // 测试正常情况
  let result1 = safe_divide(10, 2)
  @azimuth.assert_eq(true, @azimuth.is_success(result1))
  @azimuth.assert_eq(5, @azimuth.get_value(result1))
  
  // 测试错误情况
  let result2 = safe_divide(10, 0)
  @azimuth.assert_eq(false, @azimuth.is_success(result2))
  @azimuth.assert_eq_string("Division by zero", @azimuth.get_error(result2))
}