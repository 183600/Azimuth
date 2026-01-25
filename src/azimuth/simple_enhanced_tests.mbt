// 简单的增强 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

// 测试 1: 基本数学运算组合
test "basic_math_combination" {
  // 测试加法和乘法的组合
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  // 测试更复杂的组合
  let result2 = add(divide_with_ceil(10, 3), multiply(2, 2))
  assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

// 测试 2: 边界值测试
test "boundary_values" {
  // 测试零值
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  
  // 测试除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
}

// 测试 3: 字符串处理
test "string_processing" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试特殊字符
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

// 测试 4: 实际应用场景 - 购物车计算
test "shopping_cart_calculation" {
  // 计算购物车总价
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(58, subtotal1)  // 29 * 2 = 58
  assert_eq(45, subtotal2)  // 15 * 3 = 45
  assert_eq(103, total)    // 58 + 45 = 103
}

// 测试 5: 包装计算
test "packaging_calculation" {
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

// 测试 6: 负数运算
test "negative_operations" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  
  // 测试负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

// 测试 7: 大数运算
test "large_number_operations" {
  // 测试大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, add(500000, 499999))
  
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
}

// 测试 8: 数学性质验证
test "mathematical_properties" {
  let x = 10
  let y = 15
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

// 测试 9: 业务逻辑计算
test "business_logic" {
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

// 测试 10: 综合计算
test "comprehensive_calculation" {
  // 复杂的多步计算
  let base_value = 100
  let multiplier = 3
  let addition = 50
  let divisor = 4
  
  let step1 = multiply(base_value, multiplier)
  let step2 = add(step1, addition)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(300, step1)
  assert_eq(350, step2)
  assert_eq(88, result)  // ceil(350/4) = 88
}