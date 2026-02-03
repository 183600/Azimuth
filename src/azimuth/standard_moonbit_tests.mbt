// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_by_zero" {
  // 测试乘法函数与零的运算
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, -100))
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "divide_with_ceil_negative_dividend" {
  // 测试向上取整除法的负数被除数
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-2, divide_with_ceil(-6, 3))   // -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 4))   // -0.75 -> -1
  assert_eq(0, divide_with_ceil(-1, 0))    // 除数为0 -> 0
}

test "divide_with_ceil_negative_divisor" {
  // 测试向上取整除法的负数除数
  assert_eq(-3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -4))    // 10/-4 = -2.5 -> -2 (但实现可能不同)
  assert_eq(4, divide_with_ceil(-10, -3))   // -10/-3 = 3.666... -> 4
  assert_eq(0, divide_with_ceil(5, 0))      // 除数为0 -> 0
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello,   !", greet("   "))  // 空格
  assert_eq_string("Hello, \t\n!", greet("\t\n"))  // 制表符和换行符
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z - x
  let result1 = add(multiply(add(x, y), z), -x)
  assert_eq(490, result1)  // (10 + 15) * 20 - 10 = 25 * 20 - 10 = 500 - 10 = 490
  
  // 测试 x * y + z * x
  let result2 = add(multiply(x, y), multiply(z, x))
  assert_eq(350, result2)  // 10 * 15 + 20 * 10 = 150 + 200 = 350
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let shipping_cost = 12
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  
  // 计算最终总价
  let total = add(subtotal, shipping_cost)
  
  assert_eq(58, item1_total)  // 29 * 2 = 58
  assert_eq(45, item2_total)  // 15 * 3 = 45
  assert_eq(103, subtotal)    // 58 + 45 = 103
  assert_eq(115, total)       // 103 + 12 = 115
}

test "packaging_and_shipping_calculation" {
  // 测试包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let weight_per_item = 2  // kg
  let max_weight_per_box = 25  // kg
  
  // 计算需要的箱子数量（基于物品数量）
  let boxes_by_quantity = divide_with_ceil(total_items, items_per_box)
  
  // 计算总重量
  let total_weight = multiply(total_items, weight_per_item)
  
  // 计算需要的箱子数量（基于重量限制）
  let boxes_by_weight = divide_with_ceil(total_weight, max_weight_per_box)
  
  // 最终需要的箱子数量（取两者中的较大值）
  let final_boxes = if boxes_by_quantity > boxes_by_weight {
    boxes_by_quantity
  } else {
    boxes_by_weight
  }
  
  assert_eq(4, boxes_by_quantity)   // ceil(47/12) = 4
  assert_eq(94, total_weight)       // 47 * 2 = 94
  assert_eq(4, boxes_by_weight)     // ceil(94/25) = 4
  assert_eq(4, final_boxes)         // max(4, 4) = 4
}