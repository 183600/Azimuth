// 标准 MoonBit 测试用例集
// 测试核心功能模块

// 导入被测试的模块
use azimuth

// 测试1: add函数的基本加法运算
test add_basic_operations {
  // 正数加法
  azimuth.assert_eq(10, azimuth.add(3, 7))
  // 包含零的加法
  azimuth.assert_eq(5, azimuth.add(5, 0))
  azimuth.assert_eq(5, azimuth.add(0, 5))
  // 负数加法
  azimuth.assert_eq(-10, azimuth.add(-3, -7))
  // 正负数混合加法
  azimuth.assert_eq(2, azimuth.add(5, -3))
}

// 测试2: add函数的溢出边界处理
test add_overflow_boundary {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出边界
  azimuth.assert_eq(max_val, azimuth.add(max_val, 1))
  azimuth.assert_eq(max_val, azimuth.add(1000000000, 1147483648))
  
  // 负数溢出边界
  azimuth.assert_eq(min_val, azimuth.add(min_val, -1))
  azimuth.assert_eq(min_val, azimuth.add(-1000000000, -1147483649))
  
  // 最小值特殊处理
  azimuth.assert_eq(min_val, azimuth.add(min_val, min_val))
}

// 测试3: multiply函数的基本乘法运算
test multiply_basic_operations {
  // 正数乘法
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
  // 包含零的乘法
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  // 负数乘法
  azimuth.assert_eq(15, azimuth.multiply(-3, -5))
  azimuth.assert_eq(-15, azimuth.multiply(3, -5))
}

// 测试4: multiply函数的边界值处理
test multiply_edge_values {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘以1
  azimuth.assert_eq(42, azimuth.multiply(42, 1))
  azimuth.assert_eq(42, azimuth.multiply(1, 42))
  
  // 乘以-1
  azimuth.assert_eq(-42, azimuth.multiply(42, -1))
  azimuth.assert_eq(-42, azimuth.multiply(-1, 42))
  
  // 最小值乘以-1的特殊情况
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
}

// 测试5: multiply函数的溢出处理
test multiply_overflow_handling {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  azimuth.assert_eq(max_val, azimuth.multiply(50000, 50000))
  azimuth.assert_eq(max_val, azimuth.multiply(max_val, 2))
  
  // 负数溢出
  azimuth.assert_eq(min_val, azimuth.multiply(50000, -50000))
  azimuth.assert_eq(min_val, azimuth.multiply(max_val, -2))
  
  // 最小值相关溢出
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, 2))
}

// 测试6: greet函数的字符串处理
test greet_string_handling {
  // 标准输入
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  // 空字符串输入
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  // 包含空格的输入
  azimuth.assert_eq_string("Hello, Moon Bit!", azimuth.greet("Moon Bit"))
  // 特殊字符输入
  azimuth.assert_eq_string("Hello, @#$!", azimuth.greet("@#$"))
}

// 测试7: divide_with_ceil函数的整除情况
test divide_with_ceil_exact_division {
  // 正数整除
  azimuth.assert_eq(5, azimuth.divide_with_ceil(10, 2))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  
  // 负数整除
  azimuth.assert_eq(-5, azimuth.divide_with_ceil(-10, 2))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-9, 3))
  
  // 零除以正数
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
}

// 测试8: divide_with_ceil函数的向上取整
test divide_with_ceil_rounding_up {
  // 正数需要向上取整
  azimuth.assert_eq(6, azimuth.divide_with_ceil(11, 2))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(3, 2))
  
  // 负数需要向上取整（向零方向）
  azimuth.assert_eq(-5, azimuth.divide_with_ceil(-9, 2))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-8, 3))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-1, 2))
}

// 测试9: 断言函数的基本功能
test assertion_basic_functions {
  // assert_true测试
  azimuth.assert_true(true)
  azimuth.assert_true(1 > 0)
  azimuth.assert_true(5 == 5)
  
  // assert_false测试
  azimuth.assert_false(false)
  azimuth.assert_false(1 < 0)
  azimuth.assert_false(5 != 5)
}

// 测试10: 字符串断言函数
test string_assertion_functions {
  // 相同字符串
  azimuth.assert_eq_string("test", "test")
  azimuth.assert_eq_string("", "")
  azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 包含特殊字符的字符串
  azimuth.assert_eq_string("line1\nline2", "line1\nline2")
  azimuth.assert_eq_string("tab\there", "tab\there")
}