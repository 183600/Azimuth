// 标准 Azimuth 测试用例
// 覆盖核心功能的基本测试

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_overflow_handling" {
  // 加法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(max_val, add(max_val - 1000, 2000))
  assert_eq(min_val, add(min_val + 1000, -2000))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
}

test "multiply_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(-1, min_val))  // 最小值乘以-1的特殊情况
  assert_eq(0, multiply(0, max_val))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_function_test" {
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  
  // 零测试
  assert_eq(0, divide_with_ceil(0, 5))
}

test "combined_operations" {
  // 组合运算测试
  // 加法与乘法组合
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  
  // 除法与其他运算组合
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
}

test "assertion_functions" {
  // 断言函数测试
  // 布尔断言
  assert_true(true)
  assert_false(false)
  assert_true(1 == 1)
  assert_false(1 == 2)
  
  // 相等断言
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  
  // 字符串断言
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}

test "mathematical_properties" {
  // 数学性质测试
  // 加法性质（在安全范围内）
  assert_eq(add(2, 3), add(3, 2))  // 交换律
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))  // 结合律
  
  // 乘法性质（在安全范围内）
  assert_eq(multiply(2, 3), multiply(3, 2))  // 交换律
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))  // 结合律
  
  // 分配律（在安全范围内）
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 接近边界的运算
  assert_eq(2147483647, add(2147483647 - 1000, 1000))
  assert_eq(-2147483648, add(-2147483648 + 1000, -1000))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}