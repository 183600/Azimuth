// Azimuth 综合测试用例
// 覆盖边界条件、特殊情况和错误处理

test "add_minimum_value_edge_cases" {
  // 测试 add 函数的最小值边界情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值与正数相加
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val + 100, @azimuth.add(min_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(min_val, max_val))
  
  // 最小值与零相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 最小值与负数相加（应该返回最小值防止溢出）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

test "add_mixed_sign_operations" {
  // 测试 add 函数的混合符号运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大正数与大负数相加
  @azimuth.assert_eq(1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(-100, @azimuth.add(100, -200))
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  
  // 接近边界的混合运算
  @azimuth.assert_eq(max_val - 100, @azimuth.add(max_val, -100))
  @azimuth.assert_eq(min_val + 100, @azimuth.add(min_val, 100))
}

test "multiply_minimum_value_special_cases" {
  // 测试 multiply 函数的最小值特殊情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值乘以 -1 的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  
  // 最小值乘以其他值
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "multiply_large_number_operations" {
  // 测试 multiply 函数的大数乘法
  let max_val = 2147483647
  
  // 接近最大值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))  // sqrt(max_val) * (sqrt(max_val) + 1)
  @azimuth.assert_eq(max_val, @azimuth.multiply(12345, 174763))  // 应该溢出
  
  // 负数大数乘法
  @azimuth.assert_eq(-max_val, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-46340, -46341))
}

test "greet_special_character_handling" {
  // 测试 greet 函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))  // 空格
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))  // 换行和制表符
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))  // emoji
}

test "divide_with_ceil_negative_operations" {
  // 测试 divide_with_ceil 函数的负数除法
  // 负数除以正数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 3))
  
  // 正数除以负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(6, -3))
  
  // 负数除以负数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-6, -3))
}

test "divide_with_ceil_exact_divisions" {
  // 测试 divide_with_ceil 函数的精确除法
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-10, 2))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(10, -2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-10, -2))
  
  // 1的除法
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  @azimuth.assert_eq(-10, @azimuth.divide_with_ceil(-10, 1))
  
  // 自身除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(7, 7))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-7, -7))
}

test "complex_arithmetic_scenarios" {
  // 测试综合运算场景
  let a = 100
  let b = -50
  let c = 2
  
  // 复合运算
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(50, sum)
  
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(100, product)
  
  let result = @azimuth.divide_with_ceil(product, 3)
  @azimuth.assert_eq(34, result)
  
  // 链式运算
  @azimuth.assert_eq(150, @azimuth.add(@azimuth.multiply(10, 15), 0))
  @azimuth.assert_eq(25, @azimuth.divide_with_ceil(@azimuth.add(20, 30), 2))
}

test "extreme_value_combinations" {
  // 测试极端值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(715827883, @azimuth.divide_with_ceil(max_val, 3))
  
  // 最小值运算组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-715827882, @azimuth.divide_with_ceil(min_val, 3))
  
  // 极端值混合
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "boundary_value_exploration" {
  // 测试边界值探索
  // 小数值测试
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
  
  // 单位元测试
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  
  // 边界除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}