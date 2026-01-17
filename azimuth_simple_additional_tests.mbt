// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，简化版本

test "extreme_values_addition" {
  // 测试极大值和极小值加法
  let result1 = 2147483647 + 0
  let result2 = -2147483648 + 0
  let result3 = 1000000 + 1000000
  let result4 = -1000000 + -1000000
  
  // 简单验证
  if (result1 != 2147483647) {
    @builtin.abort("Test failed: extreme_values_addition 1")
  }
  if (result2 != -2147483648) {
    @builtin.abort("Test failed: extreme_values_addition 2")
  }
  if (result3 != 2000000) {
    @builtin.abort("Test failed: extreme_values_addition 3")
  }
  if (result4 != -2000000) {
    @builtin.abort("Test failed: extreme_values_addition 4")
  }
}

test "extreme_values_multiplication" {
  // 测试极大值和极小值乘法
  let result1 = 46340 * 46340
  let result2 = -46340 * 46340
  let result3 = 1000 * 1000
  let result4 = -1000 * 1000
  
  // 简单验证
  if (result1 != 2147395956) {
    @builtin.abort("Test failed: extreme_values_multiplication 1")
  }
  if (result2 != -2147395956) {
    @builtin.abort("Test failed: extreme_values_multiplication 2")
  }
  if (result3 != 1000000) {
    @builtin.abort("Test failed: extreme_values_multiplication 3")
  }
  if (result4 != -1000000) {
    @builtin.abort("Test failed: extreme_values_multiplication 4")
  }
}

test "mathematical_series" {
  // 测试数学级数计算
  
  // 等差数列求和: 2 + 4 + 6 + 8 + 10 = 30
  let arithmetic_sum = 2 + 4 + 6 + 8 + 10
  
  // 等比数列求和: 2 + 4 + 8 + 16 = 30
  let geometric_sum = 2 + 4 + 8 + 16
  
  // 简单验证
  if (arithmetic_sum != 30) {
    @builtin.abort("Test failed: mathematical_series 1")
  }
  if (geometric_sum != 30) {
    @builtin.abort("Test failed: mathematical_series 2")
  }
}

test "business_calculations" {
  // 测试商业计算场景
  
  // 计算折扣价: 原价 * 折扣率
  let original_price = 200
  let discount_rate = 8  // 80% 折扣，用整数表示
  let discounted_price = original_price * discount_rate
  
  // 计算总价: 单价 * 数量 + 运费
  let unit_price = 50
  let quantity = 3
  let shipping = 20
  let total = unit_price * quantity + shipping
  
  // 简单验证
  if (discounted_price != 1600) {
    @builtin.abort("Test failed: business_calculations 1")
  }
  if (total != 170) {
    @builtin.abort("Test failed: business_calculations 2")
  }
}

test "scientific_calculations" {
  // 测试科学计算场景
  
  // 计算正方形面积: 边长 * 边长
  let side_length = 7
  let square_area = side_length * side_length
  
  // 计算立方体表面积: 6 * 边长 * 边长
  let cube_surface_area = 6 * square_area
  
  // 简单验证
  if (square_area != 49) {
    @builtin.abort("Test failed: scientific_calculations 1")
  }
  if (cube_surface_area != 294) {
    @builtin.abort("Test failed: scientific_calculations 2")
  }
}

test "pattern_calculations" {
  // 测试模式计算
  
  // 斐波那契数列前几项的和: 1 + 1 + 2 + 3 + 5 = 12
  let fibonacci_sum = 1 + 1 + 2 + 3 + 5
  
  // 平方数序列: 1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
  let square_sum = 1 + 4 + 9 + 16
  
  // 简单验证
  if (fibonacci_sum != 12) {
    @builtin.abort("Test failed: pattern_calculations 1")
  }
  if (square_sum != 30) {
    @builtin.abort("Test failed: pattern_calculations 2")
  }
}

test "algebraic_expressions" {
  // 测试代数表达式
  
  // 计算 (x + y)² = x² + 2xy + y²
  let x = 3
  let y = 4
  let left_side = (x + y) * (x + y)
  let right_side = x * x + 2 * x * y + y * y
  
  // 计算 (x - y)² = x² - 2xy + y²
  let diff_left = (x - y) * (x - y)
  let diff_right = x * x - 2 * x * y + y * y
  
  // 简单验证
  if (left_side != right_side) {
    @builtin.abort("Test failed: algebraic_expressions 1")
  }
  if (diff_left != diff_right) {
    @builtin.abort("Test failed: algebraic_expressions 2")
  }
}

test "performance_oriented_calculations" {
  // 测试性能相关的计算
  
  // 大量重复计算
  let base = 5
  let result1 = base + base + base + base + base
  
  // 嵌套乘法计算
  let nested_result = (2 * 3) * (4 * 5)
  
  // 混合运算链
  let chain_result = 10 * 2 + (5 + 3) * 4
  
  // 简单验证
  if (result1 != 25) {
    @builtin.abort("Test failed: performance_oriented_calculations 1")
  }
  if (nested_result != 120) {
    @builtin.abort("Test failed: performance_oriented_calculations 2")
  }
  if (chain_result != 52) {
    @builtin.abort("Test failed: performance_oriented_calculations 3")
  }
}

test "string_operations" {
  // 测试字符串操作
  
  // 字符串连接
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2 + "!"
  
  // 简单验证
  if (combined != "Hello, World!") {
    @builtin.abort("Test failed: string_operations")
  }
}

test "boundary_conditions" {
  // 测试边界条件
  
  // 零值运算
  let zero_add = 0 + 42
  let zero_mul = 42 * 0
  
  // 负数运算
  let neg_add = -10 + 20
  let neg_mul = -5 * 5
  
  // 简单验证
  if (zero_add != 42) {
    @builtin.abort("Test failed: boundary_conditions 1")
  }
  if (zero_mul != 0) {
    @builtin.abort("Test failed: boundary_conditions 2")
  }
  if (neg_add != 10) {
    @builtin.abort("Test failed: boundary_conditions 3")
  }
  if (neg_mul != -25) {
    @builtin.abort("Test failed: boundary_conditions 4")
  }
}