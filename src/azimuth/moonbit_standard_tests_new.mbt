// MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

// 测试1: 基本加法功能
test "basic_addition_functionality" {
  // 测试正数加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-8, 6))
}

// 测试2: 基本乘法功能
test "basic_multiplication_functionality" {
  // 测试正数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

// 测试3: 问候函数功能
test "greet_function_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

// 测试4: 向上取整除法 - 正数情况
test "divide_with_ceil_positive_numbers" {
  // 测试能整除的情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(7, 7))
  
  // 测试需要向上取整的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

// 测试5: 向上取整除法 - 负数情况
test "divide_with_ceil_negative_numbers" {
  // 测试负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 测试除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
}

// 测试6: 组合运算测试
test "combined_operations_test" {
  // 测试加法和乘法的组合
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  // 测试更复杂的组合
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), 2)
  @azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  // 测试嵌套运算
  let result3 = @azimuth.add(@azimuth.multiply(10, @azimuth.divide_with_ceil(25, 4)), 5)
  @azimuth.assert_eq(75, result3)  // 10*7 + 5 = 70 + 5 = 75
}

// 测试7: 实际应用场景 - 购物车计算
test "shopping_cart_calculation" {
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let shipping = 10
  
  // 计算商品总价
  let subtotal = @azimuth.add(@azimuth.multiply(item1_price, item1_qty), 
                              @azimuth.multiply(item2_price, item2_qty))
  @azimuth.assert_eq(95, subtotal)  // 25*2 + 15*3 = 50 + 45 = 95
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(105, total)  // 95 + 10 = 105
}

// 测试8: 实际应用场景 - 包装计算
test "packaging_calculation" {
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 计算另一个场景
  let total_items2 = 47
  let items_per_box2 = 10
  let boxes_needed2 = @azimuth.divide_with_ceil(total_items2, items_per_box2)
  @azimuth.assert_eq(5, boxes_needed2)  // ceil(47/10) = 5
}

// 测试9: 边界值测试
test "boundary_value_test" {
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 测试单位值运算
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  
  // 测试大数运算
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

// 测试10: 数学性质验证
test "mathematical_properties_verification" {
  let a = 7
  let b = 13
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律 a * (b + b) = a * b + a * b
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, b))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, b))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 验证零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}