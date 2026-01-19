// 标准 MoonBit 测试用例
// 测试 lib.mbt 中的主要功能

// 导入被测试的模块
use azimuth

// 测试 add 函数的基本功能
test add_basic_functionality {
  // 测试正常加法
  azimuth.assert_eq(5, azimuth.add(2, 3))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-5, azimuth.add(-2, -3))
  azimuth.assert_eq(-1, azimuth.add(2, -3))
}

// 测试 add 函数的溢出处理
test add_overflow_handling {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  azimuth.assert_eq(max_val, azimuth.add(max_val, 1))
  azimuth.assert_eq(max_val, azimuth.add(max_val - 1, 2))
  
  // 测试负数溢出
  azimuth.assert_eq(min_val, azimuth.add(min_val, -1))
  azimuth.assert_eq(min_val, azimuth.add(min_val + 1, -2))
  
  // 测试最小值特殊情况
  azimuth.assert_eq(min_val, azimuth.add(min_val, min_val))
}

// 测试 multiply 函数的基本功能
test multiply_basic_functionality {
  // 测试正常乘法
  azimuth.assert_eq(6, azimuth.multiply(2, 3))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  azimuth.assert_eq(-6, azimuth.multiply(2, -3))
  azimuth.assert_eq(6, azimuth.multiply(-2, -3))
}

// 测试 multiply 函数的边界情况
test multiply_edge_cases {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘以1
  azimuth.assert_eq(5, azimuth.multiply(5, 1))
  azimuth.assert_eq(5, azimuth.multiply(1, 5))
  
  // 测试乘以-1
  azimuth.assert_eq(-5, azimuth.multiply(5, -1))
  azimuth.assert_eq(-5, azimuth.multiply(-1, 5))
  
  // 测试最小值乘以-1的特殊情况
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
}

// 测试 multiply 函数的溢出处理
test multiply_overflow_handling {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  azimuth.assert_eq(max_val, azimuth.multiply(max_val, 2))
  azimuth.assert_eq(max_val, azimuth.multiply(46341, 46341))
  
  // 测试负数溢出
  azimuth.assert_eq(min_val, azimuth.multiply(max_val, -2))
  azimuth.assert_eq(min_val, azimuth.multiply(46341, -46341))
}

// 测试 greet 函数
test greet_functionality {
  // 测试基本字符串拼接
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
}

// 测试 divide_with_ceil 函数
test divide_with_ceil_functionality {
  // 测试整除情况
  azimuth.assert_eq(3, azimuth.divide_with_ceil(6, 2))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-6, 2))
  
  // 测试需要向上取整的情况
  azimuth.assert_eq(4, azimuth.divide_with_ceil(7, 2))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(8, 3))
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(-3, 2))
}

// 测试断言函数
test assertion_functions {
  // 测试 assert_true 和 assert_false
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  
  // 测试 assert_eq_string
  azimuth.assert_eq_string("test", "test")
  azimuth.assert_eq_string("", "")
}