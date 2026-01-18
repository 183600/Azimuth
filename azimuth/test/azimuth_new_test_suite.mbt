// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，专注于边界条件和错误处理

test "add_zero_handling" {
  // 加法零值处理测试
  let result1 = @azimuth.add(0, 5)
  if (result1 != 5) {
    @builtin.abort("Test failed: add(0, 5) should be 5")
  }
  
  let result2 = @azimuth.add(5, 0)
  if (result2 != 5) {
    @builtin.abort("Test failed: add(5, 0) should be 5")
  }
  
  let result3 = @azimuth.add(0, 0)
  if (result3 != 0) {
    @builtin.abort("Test failed: add(0, 0) should be 0")
  }
  
  let result4 = @azimuth.add(0, -3)
  if (result4 != -3) {
    @builtin.abort("Test failed: add(0, -3) should be -3")
  }
  
  let result5 = @azimuth.add(-3, 0)
  if (result5 != -3) {
    @builtin.abort("Test failed: add(-3, 0) should be -3")
  }
}

test "multiply_zero_handling" {
  // 乘法零值处理测试
  let result1 = @azimuth.multiply(0, 5)
  if (result1 != 0) {
    @builtin.abort("Test failed: multiply(0, 5) should be 0")
  }
  
  let result2 = @azimuth.multiply(5, 0)
  if (result2 != 0) {
    @builtin.abort("Test failed: multiply(5, 0) should be 0")
  }
  
  let result3 = @azimuth.multiply(0, 0)
  if (result3 != 0) {
    @builtin.abort("Test failed: multiply(0, 0) should be 0")
  }
  
  let result4 = @azimuth.multiply(0, -3)
  if (result4 != 0) {
    @builtin.abort("Test failed: multiply(0, -3) should be 0")
  }
  
  let result5 = @azimuth.multiply(-3, 0)
  if (result5 != 0) {
    @builtin.abort("Test failed: multiply(-3, 0) should be 0")
  }
}

test "multiply_one_handling" {
  // 乘法一值处理测试
  let result1 = @azimuth.multiply(1, 5)
  if (result1 != 5) {
    @builtin.abort("Test failed: multiply(1, 5) should be 5")
  }
  
  let result2 = @azimuth.multiply(5, 1)
  if (result2 != 5) {
    @builtin.abort("Test failed: multiply(5, 1) should be 5")
  }
  
  let result3 = @azimuth.multiply(1, -3)
  if (result3 != -3) {
    @builtin.abort("Test failed: multiply(1, -3) should be -3")
  }
  
  let result4 = @azimuth.multiply(-3, 1)
  if (result4 != -3) {
    @builtin.abort("Test failed: multiply(-3, 1) should be -3")
  }
  
  let result5 = @azimuth.multiply(1, 1)
  if (result5 != 1) {
    @builtin.abort("Test failed: multiply(1, 1) should be 1")
  }
}

test "multiply_negative_one_handling" {
  // 乘法负一处理测试
  let result1 = @azimuth.multiply(-1, 5)
  if (result1 != -5) {
    @builtin.abort("Test failed: multiply(-1, 5) should be -5")
  }
  
  let result2 = @azimuth.multiply(5, -1)
  if (result2 != -5) {
    @builtin.abort("Test failed: multiply(5, -1) should be -5")
  }
  
  let result3 = @azimuth.multiply(-1, -3)
  if (result3 != 3) {
    @builtin.abort("Test failed: multiply(-1, -3) should be 3")
  }
  
  let result4 = @azimuth.multiply(-3, -1)
  if (result4 != 3) {
    @builtin.abort("Test failed: multiply(-3, -1) should be 3")
  }
  
  let result5 = @azimuth.multiply(-1, -1)
  if (result5 != 1) {
    @builtin.abort("Test failed: multiply(-1, -1) should be 1")
  }
}

test "min_value_addition_edge_cases" {
  // 最小值加法边界情况测试
  let min_val = -2147483648
  
  // 最小值与0相加
  let result1 = @azimuth.add(min_val, 0)
  if (result1 != min_val) {
    @builtin.abort("Test failed: add(min_val, 0) should be min_val")
  }
  
  let result2 = @azimuth.add(0, min_val)
  if (result2 != min_val) {
    @builtin.abort("Test failed: add(0, min_val) should be min_val")
  }
  
  // 最小值与正数相加
  let result3 = @azimuth.add(min_val, 1)
  if (result3 != -2147483647) {
    @builtin.abort("Test failed: add(min_val, 1) should be -2147483647")
  }
  
  let result4 = @azimuth.add(min_val, 2)
  if (result4 != -2147483646) {
    @builtin.abort("Test failed: add(min_val, 2) should be -2147483646")
  }
  
  // 最小值与负数相加（应返回最小值防止溢出）
  let result5 = @azimuth.add(min_val, -1)
  if (result5 != min_val) {
    @builtin.abort("Test failed: add(min_val, -1) should be min_val")
  }
  
  let result6 = @azimuth.add(min_val, -2)
  if (result6 != min_val) {
    @builtin.abort("Test failed: add(min_val, -2) should be min_val")
  }
}

test "min_value_multiplication_edge_cases" {
  // 最小值乘法边界情况测试
  let min_val = -2147483648
  
  // 最小值乘以1
  let result1 = @azimuth.multiply(min_val, 1)
  if (result1 != min_val) {
    @builtin.abort("Test failed: multiply(min_val, 1) should be min_val")
  }
  
  let result2 = @azimuth.multiply(1, min_val)
  if (result2 != min_val) {
    @builtin.abort("Test failed: multiply(1, min_val) should be min_val")
  }
  
  // 最小值乘以-1（应返回最小值防止溢出）
  let result3 = @azimuth.multiply(min_val, -1)
  if (result3 != min_val) {
    @builtin.abort("Test failed: multiply(min_val, -1) should be min_val")
  }
  
  let result4 = @azimuth.multiply(-1, min_val)
  if (result4 != min_val) {
    @builtin.abort("Test failed: multiply(-1, min_val) should be min_val")
  }
  
  // 最小值乘以大于1的数（应返回最小值防止溢出）
  let result5 = @azimuth.multiply(min_val, 2)
  if (result5 != min_val) {
    @builtin.abort("Test failed: multiply(min_val, 2) should be min_val")
  }
  
  let result6 = @azimuth.multiply(2, min_val)
  if (result6 != min_val) {
    @builtin.abort("Test failed: multiply(2, min_val) should be min_val")
  }
}

test "greet_function_edge_cases" {
  // 问候函数边界情况测试
  // 空字符串
  let result1 = @azimuth.greet("")
  if (result1 != "Hello, !") {
    @builtin.abort("Test failed: greet(\"\") should be \"Hello, !\"")
  }
  
  // 单字符
  let result2 = @azimuth.greet("A")
  if (result2 != "Hello, A!") {
    @builtin.abort("Test failed: greet(\"A\") should be \"Hello, A!\"")
  }
  
  let result3 = @azimuth.greet("中")
  if (result3 != "Hello, 中!") {
    @builtin.abort("Test failed: greet(\"中\") should be \"Hello, 中!\"")
  }
  
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  let result4 = @azimuth.greet(long_name)
  if (result4 != "Hello, VeryLongNameThatExceedsNormalExpectations!") {
    @builtin.abort("Test failed: greet(long_name) should be \"Hello, VeryLongNameThatExceedsNormalExpectations!\"")
  }
  
  // 特殊字符
  let result5 = @azimuth.greet("@#$%")
  if (result5 != "Hello, @#$%!") {
    @builtin.abort("Test failed: greet(\"@#$%\") should be \"Hello, @#$%!\"")
  }
  
  let result6 = @azimuth.greet("   ")  // 空格
  if (result6 != "Hello,    !") {
    @builtin.abort("Test failed: greet(\"   \") should be \"Hello,    !\"")
  }
}

test "overflow_boundary_addition" {
  // 加法溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的正数相加
  let result1 = @azimuth.add(2147483646, 1)  // max_val - 1 + 1 = max_val
  if (result1 != max_val) {
    @builtin.abort("Test failed: add(2147483646, 1) should be max_val")
  }
  
  let result2 = @azimuth.add(2147483645, 2)  // max_val - 2 + 2 = max_val
  if (result2 != max_val) {
    @builtin.abort("Test failed: add(2147483645, 2) should be max_val")
  }
  
  let result3 = @azimuth.add(2147483640, 7)  // 不溢出
  if (result3 != 2147483647) {
    @builtin.abort("Test failed: add(2147483640, 7) should be 2147483647")
  }
  
  let result4 = @azimuth.add(2147483640, 8)  // 溢出
  if (result4 != max_val) {
    @builtin.abort("Test failed: add(2147483640, 8) should be max_val")
  }
  
  // 接近最小值的负数相加
  let result5 = @azimuth.add(-2147483647, -1)  // min_val + 1 - 1 = min_val
  if (result5 != min_val) {
    @builtin.abort("Test failed: add(-2147483647, -1) should be min_val")
  }
  
  let result6 = @azimuth.add(-2147483646, -2)  // min_val + 2 - 2 = min_val
  if (result6 != min_val) {
    @builtin.abort("Test failed: add(-2147483646, -2) should be min_val")
  }
  
  let result7 = @azimuth.add(-2147483640, -1)  // 不溢出
  if (result7 != -2147483641) {
    @builtin.abort("Test failed: add(-2147483640, -1) should be -2147483641")
  }
  
  let result8 = @azimuth.add(-2147483640, -8)  // 溢出
  if (result8 != min_val) {
    @builtin.abort("Test failed: add(-2147483640, -8) should be min_val")
  }
}

test "mixed_sign_operations" {
  // 混合符号运算测试
  // 正负数加法
  let result1 = @azimuth.add(5, -5)
  if (result1 != 0) {
    @builtin.abort("Test failed: add(5, -5) should be 0")
  }
  
  let result2 = @azimuth.add(-5, 5)
  if (result2 != 0) {
    @builtin.abort("Test failed: add(-5, 5) should be 0")
  }
  
  let result3 = @azimuth.add(8, -5)
  if (result3 != 3) {
    @builtin.abort("Test failed: add(8, -5) should be 3")
  }
  
  let result4 = @azimuth.add(-8, 5)
  if (result4 != -3) {
    @builtin.abort("Test failed: add(-8, 5) should be -3")
  }
  
  // 正负数乘法
  let result5 = @azimuth.multiply(5, -5)
  if (result5 != -25) {
    @builtin.abort("Test failed: multiply(5, -5) should be -25")
  }
  
  let result6 = @azimuth.multiply(-5, 5)
  if (result6 != -25) {
    @builtin.abort("Test failed: multiply(-5, 5) should be -25")
  }
  
  let result7 = @azimuth.multiply(8, -5)
  if (result7 != -40) {
    @builtin.abort("Test failed: multiply(8, -5) should be -40")
  }
  
  let result8 = @azimuth.multiply(-8, -5)
  if (result8 != 40) {
    @builtin.abort("Test failed: multiply(-8, -5) should be 40")
  }
  
  // 零与正负数运算
  let result9 = @azimuth.add(0, -5)
  if (result9 != -5) {
    @builtin.abort("Test failed: add(0, -5) should be -5")
  }
  
  let result10 = @azimuth.add(0, 5)
  if (result10 != 5) {
    @builtin.abort("Test failed: add(0, 5) should be 5")
  }
  
  let result11 = @azimuth.multiply(0, -5)
  if (result11 != 0) {
    @builtin.abort("Test failed: multiply(0, -5) should be 0")
  }
  
  let result12 = @azimuth.multiply(0, 5)
  if (result12 != 0) {
    @builtin.abort("Test failed: multiply(0, 5) should be 0")
  }
}