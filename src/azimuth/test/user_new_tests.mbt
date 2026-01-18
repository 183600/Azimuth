// 用户新增的 MoonBit 测试用例
// 测试基本功能、边界条件和实际应用场景

test "addition_basic_cases" {
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "multiplication_basic_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  @azimuth.assert_eq(12, @azimuth.multiply(-4, -3))
}

test "greet_functionality" {
  // 问候函数测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_addition" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 12
  let y = 8
  
  // 分配律：x × (y + 3) = x × y + x × 3
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "real_world_application" {
  // 实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  // 商品总价 = 单价 × 数量
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 最终总价 = 商品总价 + 运费
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(617, total)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 特殊字符#$%!", @azimuth.greet("特殊字符#$%"))
}

test "assert_functions_verification" {
  // 断言函数验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}