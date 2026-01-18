// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，专注于边界条件和错误处理

// 导入必要的模块
use azimuth

test "add_zero_handling" {
  // 加法零值处理测试
  @assertion.assert_eq(5, azimuth.add(0, 5))?
  @assertion.assert_eq(5, azimuth.add(5, 0))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-3, azimuth.add(0, -3))?
  @assertion.assert_eq(-3, azimuth.add(-3, 0))?
}

test "multiply_zero_handling" {
  // 乘法零值处理测试
  @assertion.assert_eq(0, azimuth.multiply(0, 5))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, -3))?
  @assertion.assert_eq(0, azimuth.multiply(-3, 0))?
}

test "multiply_one_handling" {
  // 乘法一值处理测试
  @assertion.assert_eq(5, azimuth.multiply(1, 5))?
  @assertion.assert_eq(5, azimuth.multiply(5, 1))?
  @assertion.assert_eq(-3, azimuth.multiply(1, -3))?
  @assertion.assert_eq(-3, azimuth.multiply(-3, 1))?
  @assertion.assert_eq(1, azimuth.multiply(1, 1))?
}

test "multiply_negative_one_handling" {
  // 乘法负一处理测试
  @assertion.assert_eq(-5, azimuth.multiply(-1, 5))?
  @assertion.assert_eq(-5, azimuth.multiply(5, -1))?
  @assertion.assert_eq(3, azimuth.multiply(-1, -3))?
  @assertion.assert_eq(3, azimuth.multiply(-3, -1))?
  @assertion.assert_eq(1, azimuth.multiply(-1, -1))?
}

test "min_value_addition_edge_cases" {
  // 最小值加法边界情况测试
  let min_val = -2147483648
  
  // 最小值与0相加
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(0, min_val))?
  
  // 最小值与正数相加
  @assertion.assert_eq(-2147483647, azimuth.add(min_val, 1))?
  @assertion.assert_eq(-2147483646, azimuth.add(min_val, 2))?
  
  // 最小值与负数相加（应返回最小值防止溢出）
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -2))?
}

test "min_value_multiplication_edge_cases" {
  // 最小值乘法边界情况测试
  let min_val = -2147483648
  
  // 最小值乘以1
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(1, min_val))?
  
  // 最小值乘以-1（应返回最小值防止溢出）
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.multiply(-1, min_val))?
  
  // 最小值乘以大于1的数（应返回最小值防止溢出）
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(2, min_val))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 3))?
  @assertion.assert_eq(min_val, azimuth.multiply(3, min_val))?
}

test "greet_function_edge_cases" {
  // 问候函数边界情况测试
  // 空字符串
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  
  // 单字符
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))?
  @assertion.assert_eq_string("Hello, 中!", azimuth.greet("中"))?
  
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  @assertion.assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", azimuth.greet(long_name))?
  
  // 特殊字符
  @assertion.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))?
  @assertion.assert_eq_string("Hello,   !", azimuth.greet("   "))?  // 空格
}

test "overflow_boundary_addition" {
  // 加法溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的正数相加
  @assertion.assert_eq(max_val, azimuth.add(2147483646, 1))?  // max_val - 1 + 1 = max_val
  @assertion.assert_eq(max_val, azimuth.add(2147483645, 2))?  // max_val - 2 + 2 = max_val
  @assertion.assert_eq(max_val, azimuth.add(2147483640, 7))?  // 不溢出
  @assertion.assert_eq(max_val, azimuth.add(2147483640, 8))?  // 溢出
  
  // 接近最小值的负数相加
  @assertion.assert_eq(min_val, azimuth.add(-2147483647, -1))?  // min_val + 1 - 1 = min_val
  @assertion.assert_eq(min_val, azimuth.add(-2147483646, -2))?  // min_val + 2 - 2 = min_val
  @assertion.assert_eq(-2147483641, azimuth.add(-2147483640, -1))?  // 不溢出
  @assertion.assert_eq(min_val, azimuth.add(-2147483640, -8))?  // 溢出
}

test "overflow_boundary_multiplication" {
  // 乘法溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的乘法
  @assertion.assert_eq(2147483646, azimuth.multiply(1073741823, 2))?  // 不溢出
  @assertion.assert_eq(max_val, azimuth.multiply(1073741824, 2))?     // 溢出
  
  @assertion.assert_eq(46340, azimuth.multiply(46340, 1))?           // 不溢出
  @assertion.assert_eq(max_val, azimuth.multiply(46341, 46341))?     // 溢出
  
  // 负数乘法边界
  @assertion.assert_eq(-2147483646, azimuth.multiply(-1073741823, 2))?  // 不溢出
  @assertion.assert_eq(min_val, azimuth.multiply(-1073741824, 2))?     // 溢出
}

test "mixed_sign_operations" {
  // 混合符号运算测试
  // 正负数加法
  @assertion.assert_eq(0, azimuth.add(5, -5))?
  @assertion.assert_eq(0, azimuth.add(-5, 5))?
  @assertion.assert_eq(3, azimuth.add(8, -5))?
  @assertion.assert_eq(-3, azimuth.add(-8, 5))?
  
  // 正负数乘法
  @assertion.assert_eq(-25, azimuth.multiply(5, -5))?
  @assertion.assert_eq(-25, azimuth.multiply(-5, 5))?
  @assertion.assert_eq(-40, azimuth.multiply(8, -5))?
  @assertion.assert_eq(40, azimuth.multiply(-8, -5))?
  
  // 零与正负数运算
  @assertion.assert_eq(-5, azimuth.add(0, -5))?
  @assertion.assert_eq(5, azimuth.add(0, 5))?
  @assertion.assert_eq(0, azimuth.multiply(0, -5))?
  @assertion.assert_eq(0, azimuth.multiply(0, 5))?
}