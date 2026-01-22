// 综合测试套件 - 覆盖 azimuth 项目的主要功能
// 使用标准的 MoonBit 测试语法

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(3, @azimuth.add(1, 2))
  @azimuth.assert_eq(30, @azimuth.add(10, 20))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-30, @azimuth.add(-10, -20))
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(10, @azimuth.multiply(-2, -5))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(7, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}

test "complex_calculation_combinations" {
  // 测试复杂计算组合
  // 计算 (2 + 3) * 4
  let sum = @azimuth.add(2, 3)
  let result = @azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
  
  // 计算 10 + 5 * 2
  let product = @azimuth.multiply(5, 2)
  let total = @azimuth.add(10, product)
  @azimuth.assert_eq(20, total)
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  let a = 7
  let b = 13
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(product1, product2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 25
  let quantity = 3
  let shipping = 10
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(85, total)
  
  // 计算平均分数向上取整
  let scores_sum = 157
  let student_count = 10
  let average = @azimuth.divide_with_ceil(scores_sum, student_count)
  @azimuth.assert_eq(16, average)
}

test "boundary_and_extreme_values" {
  // 测试边界和极值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试极值加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.add(0, max_int))
  @azimuth.assert_eq(min_int, @azimuth.add(0, min_int))
  
  // 测试极值乘法
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
  @azimuth.assert_eq(max_int, @azimuth.multiply(1, max_int))
  @azimuth.assert_eq(min_int, @azimuth.multiply(1, min_int))
}