// Azimuth 数据验证综合测试
// 全面的数据验证功能测试

// 测试1: 基本数据类型验证
test "基本数据类型验证" {
  // 整数验证
  let int_value = 42
  assert_true(int_value >= 0)
  assert_true(int_value <= 100)
  
  // 浮点数验证
  let float_value = 3.14
  assert_true(float_value > 0.0)
  assert_true(float_value < 10.0)
  
  // 字符串验证
  let string_value = "azimuth"
  assert_eq(string_value.length(), 7)
  assert_true(string_value.contains("az"))
  
  // 布尔值验证
  let bool_value = true
  assert_true(bool_value)
}

// 测试2: 邮箱格式验证
test "邮箱格式验证" {
  // 有效邮箱
  let valid_emails = [
    "user@example.com",
    "test.email@domain.org",
    "user+tag@example.co.uk"
  ]
  
  for email in valid_emails {
    assert_true(email.contains("@"))
    assert_true(email.contains("."))
    assert_true(email.index_of("@") < email.length() - 1)
    assert_true(email.index_of(".") > email.index_of("@"))
  }
  
  // 无效邮箱
  let invalid_emails = [
    "invalid-email",
    "@example.com",
    "user@",
    "user.example.com"
  ]
  
  for email in invalid_emails {
    let has_at = email.contains("@")
    let has_dot = email.contains(".")
    let valid_format = has_at && has_dot && email.index_of("@") < email.length() - 1
    
    if has_at {
      assert_false(email.index_of(".") > email.index_of("@"))
    } else {
      assert_false(valid_format)
    }
  }
}

// 测试3: URL格式验证
test "URL格式验证" {
  // 有效URL
  let valid_urls = [
    "https://example.com",
    "http://localhost:8080",
    "https://api.example.com/v1/resource"
  ]
  
  for url in valid_urls {
    assert_true(url.starts_with("http://") || url.starts_with("https://"))
    assert_true(url.contains("."))
  }
  
  // 无效URL
  let invalid_urls = [
    "not-a-url",
    "ftp://example.com",
    "http://",
    "example.com"
  ]
  
  for url in invalid_urls {
    assert_false(url.starts_with("http://") || url.starts_with("https://"))
  }
}

// 测试4: 日期时间验证
test "日期时间验证" {
  // 有效日期格式 (YYYY-MM-DD)
  let valid_dates = [
    "2023-01-01",
    "2023-12-31",
    "2024-02-29" // 闰年
  ]
  
  for date in valid_dates {
    assert_eq(date.length(), 10)
    assert_eq(date[4], '-')
    assert_eq(date[7], '-')
    
    let year = date.substring(0, 4).to_int()
    let month = date.substring(5, 7).to_int()
    let day = date.substring(8, 10).to_int()
    
    assert_true(year >= 1900)
    assert_true(year <= 2100)
    assert_true(month >= 1)
    assert_true(month <= 12)
    assert_true(day >= 1)
    assert_true(day <= 31)
  }
}

// 测试5: JSON结构验证
test "JSON结构验证" {
  // 模拟JSON字符串
  let json_string = "{ \"name\": \"test\", \"value\": 42, \"active\": true }"
  
  // 基本JSON结构验证
  assert_true(json_string.starts_with("{"))
  assert_true(json_string.ends_with("}"))
  assert_true(json_string.contains("\"name\""))
  assert_true(json_string.contains("\"value\""))
  assert_true(json_string.contains("\"active\""))
  
  // 验证键值对格式
  assert_true(json_string.contains("\"test\""))
  assert_true(json_string.contains("42"))
  assert_true(json_string.contains("true"))
}

// 测试6: 数组范围验证
test "数组范围验证" {
  // 数值数组
  let numbers = [10, 20, 30, 40, 50]
  
  for num in numbers {
    assert_true(num >= 10)
    assert_true(num <= 50)
  }
  
  // 字符串数组
  let names = ["Alice", "Bob", "Charlie"]
  
  for name in names {
    assert_true(name.length() >= 3)
    assert_true(name.length() <= 10)
    assert_true(name.to_uppercase() == name.to_uppercase())
  }
}

// 测试7: 枚举值验证
test "枚举值验证" {
  enum Status {
    Active
    Inactive
    Pending
  }
  
  let valid_status = Status::Active
  let invalid_value = "invalid"
  
  match valid_status {
    Status::Active => assert_true(true)
    Status::Inactive => assert_true(false)
    Status::Pending => assert_true(false)
  }
  
  // 验证字符串是否匹配枚举值
  let status_string = "Active"
  assert_true(status_string == "Active" || status_string == "Inactive" || status_string == "Pending")
  assert_false(invalid_value == "Active" || invalid_value == "Inactive" || invalid_value == "Pending")
}

// 测试8: 复合数据结构验证
test "复合数据结构验证" {
  // 用户数据结构
  let user = {
    name: "John Doe",
    age: 30,
    email: "john@example.com",
    active: true,
    roles: ["user", "admin"]
  }
  
  // 验证用户数据
  assert_true(user.name.length() > 0)
  assert_true(user.age >= 18)
  assert_true(user.age <= 100)
  assert_true(user.email.contains("@"))
  assert_true(user.active == true || user.active == false)
  assert_true(user.roles.length() > 0)
}

// 测试9: 边界条件验证
test "边界条件验证" {
  // 数值边界
  let zero = 0
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(zero, 0)
  assert_true(max_int > 0)
  assert_true(min_int < 0)
  
  // 字符串边界
  let empty_string = ""
  let long_string = "a" * 1000
  
  assert_eq(empty_string.length(), 0)
  assert_eq(long_string.length(), 1000)
  
  // 数组边界
  let empty_array = []
  let single_element = [42]
  
  assert_eq(empty_array.length(), 0)
  assert_eq(single_element.length(), 1)
  assert_eq(single_element[0], 42)
}

// 测试10: 自定义验证规则
test "自定义验证规则" {
  // 自定义密码验证规则
  let passwords = [
    { password: "Password123", valid: true },
    { password: "weak", valid: false },
    { password: "12345678", valid: false },
    { password: "StrongPass!", valid: true }
  ]
  
  for pwd in passwords {
    let has_upper = pwd.password.to_uppercase() != pwd.password
    let has_lower = pwd.password.to_lowercase() != pwd.password
    let has_digit = false
    let has_special = false
    let long_enough = pwd.password.length() >= 8
    
    // 检查数字
    for ch in pwd.password.to_char_array() {
      if ch >= '0' && ch <= '9' {
        has_digit = true
      }
      if (ch >= '!' && ch <= '/') || (ch >= ':' && ch <= '@') {
        has_special = true
      }
    }
    
    let is_valid = has_upper && has_lower && has_digit && long_enough
    
    if pwd.valid {
      assert_true(is_valid)
    } else {
      // 对于无效密码，至少违反一个规则
      assert_false(has_upper && has_lower && has_digit && long_enough)
    }
  }
}