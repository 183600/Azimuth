// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(100, azimuth::add(50, 50))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(0, azimuth::multiply(0, 10))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
  azimuth::assert_eq(100, azimuth::multiply(10, 10))
}

test "greet_function_basic" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-4, 2))
}

test "zero_operations" {
  // 测试与零相关的运算
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
  azimuth::assert_eq(5, azimuth::add(5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
}

test "combined_operations" {
  // 测试组合运算
  let result = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth::multiply(azimuth::add(5, 3), 2)
  azimuth::assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "practical_packaging" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  azimuth::assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "greeting_unicode_support" {
  // 测试 Unicode 字符支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, αβγ!", azimuth::greet("αβγ"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 15
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(x, 0))
  azimuth::assert_eq(x, azimuth::add(x, 0))
}