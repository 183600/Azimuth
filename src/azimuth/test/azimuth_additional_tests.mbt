// Azimuth 额外测试用例 - 使用标准 MoonBit 测试语法
// 测试核心函数的各种边界情况和实际应用场景

test "add_boundary_values" {
  // 测试加法的边界值
  azimuth::assert_eq(2147483647, azimuth::add(2147483646, 1))  // 接近 Int 最大值
  azimuth::assert_eq(-2147483648, azimuth::add(-2147483647, -1))  // 接近 Int 最小值
  azimuth::assert_eq(0, azimuth::add(2147483647, -2147483647))  // 最大值与相反数
}

test "multiply_boundary_values" {
  // 测试乘法的边界值
  azimuth::assert_eq(2147483647, azimuth::multiply(2147483647, 1))  // 最大值乘以1
  azimuth::assert_eq(-2147483648, azimuth::multiply(-2147483648, 1))  // 最小值乘以1
  azimuth::assert_eq(0, azimuth::multiply(2147483647, 0))  // 最大值乘以0
}

test "divide_with_ceil_special_cases" {
  // 测试向上取整除法的特殊情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))  // 零除以正数
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, -1))  // 零除以负数
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))  // 零除以零
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))  // 相等的数
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-1, -1))  // 相等的负数
}

test "greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  azimuth::assert_eq_string("Hello, 🌟!", azimuth::greet("🌟"))  // Emoji
  azimuth::assert_eq_string("Hello, αβγ!", azimuth::greet("αβγ"))  // 希腊字母
  azimuth::assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))  // 阿拉伯文
  azimuth::assert_eq_string("Hello, ñáéíóú!", azimuth::greet("ñáéíóú"))  // 带重音符号
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算
  let a = 12
  let b = 8
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = azimuth::multiply(azimuth::add(a, b), c)
  azimuth::assert_eq(100, result1)  // (12 + 8) * 5 = 20 * 5 = 100
  
  // 测试 a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  azimuth::assert_eq(101, result2)  // 12 * 8 + 5 = 96 + 5 = 101
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-15, azimuth::add(-8, -7))  // 负数加法
  azimuth::assert_eq(20, azimuth::multiply(-4, -5))  // 负数乘法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))  // 负数向上取整除法
  azimuth::assert_eq(3, azimuth::divide_with_ceil(-10, -3))  // 负数除以负数
}

test "zero_value_operations" {
  // 测试零值运算
  azimuth::assert_eq(0, azimuth::multiply(0, 12345))  // 零乘以任意数
  azimuth::assert_eq(12345, azimuth::add(12345, 0))  // 任意数加零
  azimuth::assert_eq(-12345, azimuth::add(-12345, 0))  // 负数加零
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 12345))  // 零除以任意数
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let discount = 10
  
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let total_before_discount = azimuth::add(subtotal1, subtotal2)
  let final_total = azimuth::add(total_before_discount, -discount)
  
  azimuth::assert_eq(58, subtotal1)  // 29 * 2 = 58
  azimuth::assert_eq(45, subtotal2)  // 15 * 3 = 45
  azimuth::assert_eq(103, total_before_discount)  // 58 + 45 = 103
  azimuth::assert_eq(93, final_total)  // 103 - 10 = 93
}

test "packaging_optimization" {
  // 测试包装优化计算
  let total_products = 47
  let box_capacity = 12
  let warehouse_cost_per_box = 5
  
  let boxes_needed = azimuth::divide_with_ceil(total_products, box_capacity)
  let total_cost = azimuth::multiply(boxes_needed, warehouse_cost_per_box)
  
  azimuth::assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  azimuth::assert_eq(20, total_cost)  // 4 * 5 = 20
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 分配律：x * (y + y) = x * y + x * y
  let distributive_left = azimuth::multiply(x, azimuth::add(y, y))
  let distributive_right = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, y))
  azimuth::assert_eq(distributive_left, distributive_right)
}