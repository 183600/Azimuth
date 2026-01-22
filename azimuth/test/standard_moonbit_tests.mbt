// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-4, @azimuth.multiply(-2, 2))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "divide_with_ceil_function_basic_functionality" {
  // 测试基本向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  let x = 42
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(45, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(53, result2)
}

test "boundary_value_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(3, @azimuth.add(-5, 8))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let total = @azimuth.add(@azimuth.add(item1, item2), item3)
  @azimuth.assert_eq(597, total)
  
  // 场景2：计算批量订购需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 场景3：生成个性化问候
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}