// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_tests" {
  // 基本加法测试
  inspect(add(0, 0), "0")?
  inspect(add(2, 3), "5")?
  inspect(add(-2, 1), "-1")?
  inspect(add(-2, -3), "-5")?
  inspect(add(50, 50), "100")?
}

test "add_overflow_tests" {
  // 加法溢出测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  inspect(add(max_val, 0), "2147483647")?
  inspect(add(min_val, 0), "-2147483648")?
  
  // 溢出保护测试
  inspect(add(max_val, 1), "2147483647")?
  inspect(add(max_val, max_val), "2147483647")?
  inspect(add(min_val, -1), "-2147483648")?
}

test "multiply_basic_tests" {
  // 基本乘法测试
  inspect(multiply(0, 5), "0")?
  inspect(multiply(5, 0), "0")?
  inspect(multiply(2, 3), "6")?
  inspect(multiply(2, -3), "-6")?
  inspect(multiply(-2, -3), "6")?
  inspect(multiply(3, 5), "15")?
}

test "multiply_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 1和-1的特殊情况
  inspect(multiply(1, 5), "5")?
  inspect(multiply(-1, 5), "-5")?
  inspect(multiply(-1, min_val), "-2147483648")?  // 最小值乘以-1的特殊情况
  
  // 边界值测试
  inspect(multiply(1, max_val), "2147483647")?
  inspect(multiply(1, min_val), "-2147483648")?
}

test "greet_function_tests" {
  // greet函数测试
  inspect(greet("World"), "Hello, World!")?
  inspect(greet("MoonBit"), "Hello, MoonBit!")?
  inspect(greet(""), "Hello, !")?
  inspect(greet("张三"), "Hello, 张三!")?
  inspect(greet("123"), "Hello, 123!")?
}

test "divide_with_ceil_tests" {
  // 向上取整除法测试
  // 正数测试
  inspect(divide_with_ceil(11, 3), "4")?
  inspect(divide_with_ceil(12, 3), "4")?
  inspect(divide_with_ceil(5, 3), "2")?
  inspect(divide_with_ceil(1, 3), "1")?
  
  // 负数测试
  inspect(divide_with_ceil(-8, 3), "-3")?
  inspect(divide_with_ceil(-6, 3), "-2")?
  inspect(divide_with_ceil(-2, 3), "-1")?
  
  // 零测试
  inspect(divide_with_ceil(0, 5), "0")?
}

test "combined_operations_tests" {
  // 组合运算测试
  // 加法和乘法组合
  inspect(add(multiply(3, 5), multiply(2, 5)), "25")?
  inspect(multiply(add(3, 2), 4), "20")?
  
  // 复杂表达式
  inspect(divide_with_ceil(add(multiply(2, 3), 2), 2), "8")?
  inspect(add(multiply(divide_with_ceil(10, 2), 2), 5), "15")?
  
  // 链式运算
  inspect(add(multiply(2, 5), divide_with_ceil(10, 3)), "16")?
}

test "mathematical_properties_tests" {
  // 数学性质测试
  // 加法交换律（在安全范围内）
  inspect(add(5, 3), "8")?
  inspect(add(3, 5), "8")?
  inspect(add(-5, 3), "-2")?
  inspect(add(3, -5), "-2")?
  
  // 乘法交换律（在安全范围内）
  inspect(multiply(5, 3), "15")?
  inspect(multiply(3, 5), "15")?
  inspect(multiply(-5, 3), "-15")?
  inspect(multiply(3, -5), "-15")?
  
  // 分配律（在安全范围内）
  inspect(multiply(2, add(3, 4)), "14")?
  inspect(add(multiply(2, 3), multiply(2, 4)), "14")?
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  inspect(add(max_val, min_val), "-1")?
  inspect(add(min_val, max_val), "-1")?
  
  // 极值乘法
  inspect(multiply(max_val, 0), "0")?
  inspect(multiply(min_val, 0), "0")?
  
  // 接近边界的运算
  inspect(add(max_val - 100, 100), "2147483647")?
  inspect(add(min_val + 100, -100), "-2147483648")?
}