// 标准增强测试套件 - 测试 azimuth 包的核心功能
// 使用标准 MoonBit 测试语法编写

test "add_basic_arithmetic" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(-1, add(2, -3))
}

test "add_overflow_protection" {
  // 加法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "multiply_basic_operations" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "multiply_boundary_values" {
  // 乘法边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_string_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  
  // 零除数测试（会触发运行时错误）
  // assert_eq(0, divide_with_ceil(5, 0)) // 这行会抛出异常
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let a = 10
  let b = 3
  let c = -2
  
  // 组合运算
  assert_eq(39, multiply(add(a, b), 3))  // (10+3)*3 = 39
  assert_eq(24, add(multiply(a, b), c))  // 10*3+(-2) = 28
  assert_eq(13, add(multiply(a, b), -17)) // 10*3+(-17) = 13
  
  // 嵌套运算
  assert_eq(16, multiply(add(2, 2), add(2, 2)))  // (2+2)*(2+2) = 16
  assert_eq(8, add(multiply(2, 3), add(1, 1)))   // 2*3+(1+1) = 8
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  // 交换律
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(multiply(5, 3), multiply(3, 5))
  
  // 结合律（在安全范围内）
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))
  
  // 分配律（在安全范围内）
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
  
  // 单位元
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
}

test "edge_cases_and_special_values" {
  // 边界情况和特殊值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值组合
  assert_eq(max_val - 1, add(max_val, min_val))
  assert_eq(min_val + 1, add(min_val, max_val))
  
  // 自反运算
  assert_eq(0, add(5, -5))
  assert_eq(0, multiply(5, 0))
  
  // 特殊字符串情况
  assert_eq_string("Hello,   !", greet("   "))  // 空格
  assert_eq_string("Hello, \n\t!", greet("\n\t"))  // 控制字符
  assert_eq_string("Hello, 中文!", greet("中文"))  // 非ASCII字符
}