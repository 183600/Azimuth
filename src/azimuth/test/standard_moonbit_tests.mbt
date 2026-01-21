// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 2)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2))
  )
}

test "edge_cases" {
  // 测试边界情况
  // 零值测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 负数测试
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (2 + 3) * 4 = 20
  let sum = @azimuth.add(2, 3)
  let result = @azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
  
  // 计算购物车总价
  let item1_total = @azimuth.multiply(10, 2)  // 10元商品，2件
  let item2_total = @azimuth.multiply(15, 3)  // 15元商品，3件
  let total = @azimuth.add(item1_total, item2_total)
  @azimuth.assert_eq(65, total)
}

test "string_processing" {
  // 测试字符串处理
  // 特殊字符
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  
  // 长字符串
  let long_name = "VeryLongNameWithManyCharacters"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "business_calculations" {
  // 测试业务计算场景
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 计算订单总价（含税费）
  let subtotal = @azimuth.multiply(100, 3)  // 100元商品，3件
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, 8), 100)  // 8%税费
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(324, total)
}