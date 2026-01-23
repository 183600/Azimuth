// 标准 MoonBit 测试套件
// 包含10个核心测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法测试
  assert_eq(22, a + b)
  
  // 减法测试
  assert_eq(8, a - b)
  
  // 乘法测试
  assert_eq(105, a * b)
  
  // 除法测试
  assert_eq(2, a / b)
  
  // 取模测试
  assert_eq(1, a % b)
}

test "string_operations" {
  // 测试字符串操作
  let greeting = "Hello"
  let name = "MoonBit"
  
  // 字符串连接
  let full_greeting = greeting + ", " + name + "!"
  assert_eq_string("Hello, MoonBit!", full_greeting)
  
  // 空字符串处理
  let empty = ""
  let result = empty + "test"
  assert_eq_string("test", result)
}

test "boolean_logic" {
  // 测试布尔逻辑
  let x = true
  let y = false
  
  // 逻辑与
  assert_eq(true, x && x)
  assert_eq(false, x && y)
  
  // 逻辑或
  assert_eq(true, x || y)
  assert_eq(false, y || y)
  
  // 逻辑非
  assert_eq(false, !x)
  assert_eq(true, !y)
}

test "comparison_operations" {
  // 测试比较运算
  let small = 5
  let medium = 10
  let large = 15
  
  // 等于和不等于
  assert_eq(true, medium == medium)
  assert_eq(false, small == large)
  assert_eq(true, small != large)
  
  // 大于和小于
  assert_eq(true, large > medium)
  assert_eq(true, small < medium)
  
  // 大于等于和小于等于
  assert_eq(true, medium >= small)
  assert_eq(true, medium <= large)
}

test "conditional_expressions" {
  // 测试条件表达式
  let age = 18
  
  // 简单条件
  let is_adult = if (age >= 18) { true } else { false }
  assert_eq(true, is_adult)
  
  // 嵌套条件
  let category = if (age < 13) {
    "child"
  } else if (age < 18) {
    "teenager"
  } else {
    "adult"
  }
  assert_eq_string("adult", category)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 数组长度
  assert_eq(5, numbers.length())
  
  // 数组索引访问
  assert_eq(1, numbers[0])
  assert_eq(3, numbers[2])
  assert_eq(5, numbers[4])
  
  // 数组边界测试
  let single_element = [42]
  assert_eq(1, single_element.length())
  assert_eq(42, single_element[0])
}

test "function_definitions" {
  // 测试函数定义和调用
  // 简单函数
  let square = fn(x) { x * x }
  assert_eq(16, square(4))
  assert_eq(0, square(0))
  
  // 多参数函数
  let add_and_double = fn(a, b) { (a + b) * 2 }
  assert_eq(10, add_and_double(2, 3))
  
  // 函数组合
  let result = square(add_and_double(1, 2))
  assert_eq(36, result)  // (1+2)*2 = 6, 6^2 = 36
}

test "numeric_boundaries" {
  // 测试数值边界情况
  // 零值测试
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 5)
  assert_eq(0, 5 * 0)
  
  // 负数测试
  assert_eq(-5, -2 + -3)
  assert_eq(6, -2 * -3)
  assert_eq(-6, 2 * -3)
  
  // 单位元测试
  assert_eq(7, 7 + 0)
  assert_eq(7, 7 * 1)
  assert_eq(7, 7 / 1)
}

test "string_with_numbers" {
  // 测试包含数字的字符串操作
  let prefix = "Value: "
  let number = 42
  let suffix = " units"
  
  // 数字转字符串并连接
  let result = prefix + number.to_string() + suffix
  assert_eq_string("Value: 42 units", result)
  
  // 特殊字符处理
  let special = "Test@123#"
  assert_eq_string("Hello, Test@123#!", "Hello, " + special + "!")
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 安全除法函数
  let safe_divide = fn(dividend, divisor) {
    if (divisor == 0) {
      0  // 返回0而不是出错
    } else {
      dividend / divisor
    }
  }
  
  // 正常除法
  assert_eq(5, safe_divide(10, 2))
  
  // 除零保护
  assert_eq(0, safe_divide(10, 0))
  assert_eq(0, safe_divide(0, 0))
  
  // 负数除法
  assert_eq(-3, safe_divide(-10, 3))
}