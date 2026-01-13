// 高级 MoonBit 测试用例
// 测试 azimuth 包中的高级功能和边界情况

// 测试加法的交换律
test "add_commutative_property" {
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-15, 27), @azimuth.add(27, -15))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
}

// 测试加法的结合律
test "add_associative_property" {
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(10, 20), 30), @azimuth.add(10, @azimuth.add(20, 30)))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(-10, 20), 30), @azimuth.add(-10, @azimuth.add(20, 30)))
}

// 测试乘法的交换律
test "multiply_commutative_property" {
  @azimuth.assert_eq(@azimuth.multiply(12, 8), @azimuth.multiply(8, 12))
  @azimuth.assert_eq(@azimuth.multiply(-12, 8), @azimuth.multiply(8, -12))
  @azimuth.assert_eq(@azimuth.multiply(-12, -8), @azimuth.multiply(-8, -12))
}

// 测试乘法的结合律
test "multiply_associative_property" {
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(3, 4), 5), @azimuth.multiply(3, @azimuth.multiply(4, 5)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(-3, 4), 5), @azimuth.multiply(-3, @azimuth.multiply(4, 5)))
}

// 测试乘法对加法的分配律
test "multiply_distributive_property" {
  @azimuth.assert_eq(@azimuth.multiply(3, @azimuth.add(4, 5)), @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(3, 5)))
  @azimuth.assert_eq(@azimuth.multiply(-3, @azimuth.add(4, 5)), @azimuth.add(@azimuth.multiply(-3, 4), @azimuth.multiply(-3, 5)))
}

// 测试大数运算
test "large_number_operations" {
  let large_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(large_num, large_num))
}

// 测试混合运算场景
test "mixed_operations_scenario" {
  // 计算 (2 + 3) * 4 - 5
  let step1 = @azimuth.add(2, 3)
  let step2 = @azimuth.multiply(step1, 4)
  let result = @azimuth.add(step2, -5)  // 使用加负数代替减法
  @azimuth.assert_eq(15, result)
}

// 测试字符串拼接的复杂场景
test "complex_string_scenarios" {
  let long_name = "VeryLongNameThatContainsManyCharactersAndNumbers123456789"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatContainsManyCharactersAndNumbers123456789!", result)
  
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result2)
}

// 测试边界值的组合运算
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 (max_val + min_val) + 1
  let step1 = @azimuth.add(max_val, min_val)
  let result = @azimuth.add(step1, 1)
  @azimuth.assert_eq(0, result)
  
  // 测试 min_val * 0
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试 max_val * 1
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

// 测试实际应用场景
test "real_world_application" {
  // 模拟计算购物车总价：商品价格 * 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
  
  // 测试用户欢迎消息
  let username = "张三"
  let welcome_msg = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_msg)
}