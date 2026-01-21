// 标准 Azimuth 测试套件
// 包含10个标准测试用例，使用 MoonBit 标准测试语法

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(1000, 0))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的全面情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(1, 15))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种变化
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "divide_with_ceil_normal_cases" {
  // 测试 divide_with_ceil 函数的正常情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 10
  let b = 20
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "real_world_shopping_scenario" {
  // 测试真实世界购物场景
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  
  let item1_total = @azimuth.multiply(item1_price, item1_qty)
  let item2_total = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(item1_total, item2_total)
  
  @azimuth.assert_eq(103, total)  // 29*2 + 15*3 = 58 + 45 = 103
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "complex_calculations" {
  // 测试复杂计算
  let complex_result = @azimuth.add(
    @azimuth.multiply(10, @azimuth.divide_with_ceil(25, 4)),
    @azimuth.multiply(5, @azimuth.divide_with_ceil(17, 3))
  )
  
  @azimuth.assert_eq(85, complex_result)  // 10*7 + 5*6 = 70 + 15 = 85
}

test "edge_cases_and_robustness" {
  // 测试边界情况和健壮性
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}