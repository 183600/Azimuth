// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 导入被测试的模块
use azimuth

// 测试 add 函数的边界值
test add_boundary_values {
  // 测试大数相加
  azimuth.assert_eq(1000000, azimuth.add(500000, 500000))
  azimuth.assert_eq(-1000000, azimuth.add(-500000, -500000))
  
  // 测试混合符号
  azimuth.assert_eq(0, azimuth.add(100, -100))
  azimuth.assert_eq(-50, azimuth.add(-100, 50))
}

// 测试 multiply 函数的边界值
test multiply_boundary_values {
  // 测试大数相乘
  azimuth.assert_eq(1000000, azimuth.multiply(1000, 1000))
  azimuth.assert_eq(-1000000, azimuth.multiply(1000, -1000))
  
  // 测试与0相乘
  azimuth.assert_eq(0, azimuth.multiply(12345, 0))
  azimuth.assert_eq(0, azimuth.multiply(-12345, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 0))
}

// 测试 greet 函数的特殊字符处理
test greet_special_characters {
  // 测试包含特殊字符的名称
  azimuth.assert_eq_string("Hello, John-Doe!", azimuth.greet("John-Doe"))
  azimuth.assert_eq_string("Hello, O'Reilly!", azimuth.greet("O'Reilly"))
  azimuth.assert_eq_string("Hello, @moonbit!", azimuth.greet("@moonbit"))
}

// 测试 divide_with_ceil 函数的边界情况
test divide_with_ceil_edge_cases {
  // 测试除数为1和-1
  azimuth.assert_eq(5, azimuth.divide_with_ceil(5, 1))
  azimuth.assert_eq(-5, azimuth.divide_with_ceil(-5, 1))
  azimuth.assert_eq(-5, azimuth.divide_with_ceil(5, -1))
  azimuth.assert_eq(5, azimuth.divide_with_ceil(-5, -1))
  
  // 测试除数为0的情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-5, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
}

// 测试负数除法的向上取整
test divide_with_ceil_negative_numbers {
  // 测试负数除法的特殊情况
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-7, 2))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(-7, -2))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(7, -2))
  
  // 测试边界情况
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-1, 2))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, -2))
}

// 测试组合运算
test combined_operations {
  // 测试加法和乘法的组合
  let result1 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  azimuth.assert_eq(26, result1)
  
  // 测试嵌套运算
  let result2 = azimuth.multiply(azimuth.add(2, 3), azimuth.add(4, 5))
  azimuth.assert_eq(45, result2)
}

// 测试字符串处理边界情况
test greet_edge_cases {
  // 测试长字符串
  let long_name = "This is a very long name that contains many words and characters"
  let expected = "Hello, " + long_name + "!"
  azimuth.assert_eq_string(expected, azimuth.greet(long_name))
  
  // 测试包含空格的名称
  azimuth.assert_eq_string("Hello, John Smith!", azimuth.greet("John Smith"))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("   "))
}

// 测试数值运算的恒等性
test arithmetic_identities {
  // 加法恒等性
  azimuth.assert_eq(10, azimuth.add(10, 0))
  azimuth.assert_eq(10, azimuth.add(0, 10))
  
  // 乘法恒等性
  azimuth.assert_eq(10, azimuth.multiply(10, 1))
  azimuth.assert_eq(10, azimuth.multiply(1, 10))
  
  // 乘法零元素
  azimuth.assert_eq(0, azimuth.multiply(10, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 10))
}

// 测试除法的数学性质
test divide_with_ceil_properties {
  // 测试除法的单调性
  azimuth.assert_true(azimuth.divide_with_ceil(10, 3) >= azimuth.divide_with_ceil(9, 3))
  azimuth.assert_true(azimuth.divide_with_ceil(10, 3) >= azimuth.divide_with_ceil(10, 4))
  
  // 测试整除性质
  azimuth.assert_eq(azimuth.divide_with_ceil(12, 3), azimuth.divide_with_ceil(12, 4))
}