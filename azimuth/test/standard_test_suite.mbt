// 标准测试用例 - 使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  inspect(@azimuth.add(0, 0), "0")?
  inspect(@azimuth.add(2, 3), "5")?
  inspect(@azimuth.add(-2, 1), "-1")?
  inspect(@azimuth.add(50, 50), "100")?
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  inspect(@azimuth.multiply(0, 5), "0")?
  inspect(@azimuth.multiply(2, 3), "6")?
  inspect(@azimuth.multiply(2, -3), "-6")?
  inspect(@azimuth.multiply(5, 5), "25")?
}

test "greet_function_string_handling" {
  // 测试问候函数的字符串处理
  inspect(@azimuth.greet("World"), "Hello, World!")?
  inspect(@azimuth.greet(""), "Hello, !")?
  inspect(@azimuth.greet("MoonBit"), "Hello, MoonBit!")?
  inspect(@azimuth.greet("123"), "Hello, 123!")?
}

test "divide_with_ceil_function_basic_cases" {
  // 测试向上取整除法函数的基本情况
  inspect(@azimuth.divide_with_ceil(11, 3), "4")?
  inspect(@azimuth.divide_with_ceil(12, 3), "4")?
  inspect(@azimuth.divide_with_ceil(13, 3), "5")?
  inspect(@azimuth.divide_with_ceil(0, 5), "0")?
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 15
  let b = 25
  
  // 加法交换律
  inspect(@azimuth.add(a, b), @azimuth.add(b, a))?
  
  // 乘法交换律
  inspect(@azimuth.multiply(a, b), @azimuth.multiply(b, a))?
}

test "edge_cases_zero_values" {
  // 测试零值的边界情况
  let test_val = 42
  
  // 加法零值测试
  inspect(@azimuth.add(test_val, 0), "42")?
  inspect(@azimuth.add(0, test_val), "42")?
  
  // 乘法零值测试
  inspect(@azimuth.multiply(test_val, 0), "0")?
  inspect(@azimuth.multiply(0, test_val), "0")?
  
  // 乘法单位元测试
  inspect(@azimuth.multiply(test_val, 1), "42")?
  inspect(@azimuth.multiply(1, test_val), "42")?
}

test "negative_number_operations" {
  // 测试负数运算
  inspect(@azimuth.add(-2, -3), "-5")?
  inspect(@azimuth.add(-2, 3), "1")?
  inspect(@azimuth.multiply(-2, -3), "6")?
  inspect(@azimuth.multiply(2, -3), "-6")?
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  inspect(@azimuth.divide_with_ceil(-8, 3), "-3")?
  inspect(@azimuth.divide_with_ceil(-9, 3), "-3")?
  inspect(@azimuth.divide_with_ceil(-10, 3), "-4")?
  inspect(@azimuth.divide_with_ceil(-10, -3), "3")?
}