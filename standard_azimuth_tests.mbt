// 标准 MoonBit 测试用例 - Azimuth 项目
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

// 导入 azimuth 库
use azimuth

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-5, azimuth.add(-2, -3))
  assert_eq(1000, azimuth.add(500, 500))
  assert_eq(0, azimuth.add(10, -10))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-6, azimuth.multiply(-2, 3))
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(100, azimuth.multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 中文!", azimuth.greet("中文"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 100))
  assert_eq(2, azimuth.divide_with_ceil(3, 2))
  assert_eq(34, azimuth.divide_with_ceil(100, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  assert_eq(-2, azimuth.divide_with_ceil(-4, 2))
  assert_eq(0, azimuth.divide_with_ceil(5, 0))
  assert_eq(-1, azimuth.divide_with_ceil(-1, 2))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 零元素性质
  assert_eq(0, azimuth.multiply(a, 0))
  assert_eq(a, azimuth.add(a, 0))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let result1 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth.multiply(azimuth.add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = azimuth.divide_with_ceil(azimuth.add(10, 5), 4)
  assert_eq(4, result3)  // ceil(15/4) = 4
}

test "real_world_business_scenario" {
  // 实际业务场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let tax_rate = 8
  
  let subtotal = azimuth.multiply(unit_price, quantity)
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  let total = azimuth.add(azimuth.add(subtotal, shipping), tax)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_and_distribution_calculation" {
  // 包装和分销计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 4
  
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  let pallets_needed = azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(1, pallets_needed)  // ceil(4/4) = 1
}

test "unicode_and_special_characters_handling" {
  // 测试 Unicode 和特殊字符处理
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, αβγ!", azimuth.greet("αβγ"))
  assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))
  assert_eq_string("Hello, Café!", azimuth.greet("Café"))
}