// 标准 MoonBit 测试用例 - Azimuth 库
// 包含10个高质量测试用例，涵盖基本功能和边界情况

// 测试1: 基本加法功能
test "basic_addition_functionality" {
  // 测试基本的加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

// 测试2: 基本乘法功能
test "basic_multiplication_functionality" {
  // 测试基本的乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

// 测试3: 向上取整除法的正数情况
test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))   // 1.5 -> 2
}

// 测试4: 向上取整除法的负数和零值情况
test "divide_with_ceil_negative_and_zero" {
  // 测试负数和零值的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为零返回0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 零除以任何数返回0
}

// 测试5: 问候函数的基本功能
test "greet_function_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串测试
}

// 测试6: 问候函数的Unicode支持
test "greet_function_unicode_support" {
  // 测试Unicode字符和特殊符号的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试7: 数学交换律验证
test "mathematical_commutative_laws" {
  // 验证加法和乘法交换律
  let a = 7
  let b = 13
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

// 测试8: 实际业务场景 - 购物计算
test "shopping_cart_calculation" {
  // 模拟购物车总价计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  let shipping = 10
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 75
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 30
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)  // 105
  
  // 加上运费
  let final_total = @azimuth.add(subtotal, shipping)  // 115
  
  @azimuth.assert_eq(75, total1)
  @azimuth.assert_eq(30, total2)
  @azimuth.assert_eq(105, subtotal)
  @azimuth.assert_eq(115, final_total)
}

// 测试9: 包装计算场景
test "packaging_calculation_scenario" {
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 计算包装成本
  let cost_per_box = 3
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(15, total_cost)  // 5 * 3 = 15
}

// 测试10: 复杂表达式计算
test "complex_expression_evaluation" {
  // 测试复杂表达式的计算
  let x = 8
  let y = 12
  let z = 5
  
  // 计算 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(100, result1)  // (8 + 12) * 5 = 20 * 5 = 100
  
  // 计算 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(101, result2)  // 8 * 12 + 5 = 96 + 5 = 101
  
  // 计算组合使用除法的表达式
  let result3 = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(x, y), z), z)
  @azimuth.assert_eq(24, result3)  // ceil(8*12/5) + 5 = ceil(96/5) + 5 = 20 + 5 = 25
}