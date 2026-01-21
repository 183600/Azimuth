// Azimuth 新增综合测试用例
// 使用标准的 MoonBit 测试语法
// 包含实际应用场景和边界情况测试

test "add_financial_calculations" {
  // 测试金融计算场景
  let price1 = 1299
  let price2 = 899
  let price3 = 599
  let discount = 200
  
  let subtotal = @azimuth.add(@azimuth.add(price1, price2), price3)
  let final_total = @azimuth.add(subtotal, -discount)
  @azimuth.assert_eq(2597, final_total)
}

test "multiply_inventory_management" {
  // 测试库存管理场景
  let units_per_box = 24
  let number_of_boxes = 15
  let damaged_units = 8
  
  let total_units = @azimuth.multiply(units_per_box, number_of_boxes)
  let available_units = @azimuth.add(total_units, -damaged_units)
  @azimuth.assert_eq(352, available_units)
}

test "greet_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
}

test "divide_with_ceil_resource_allocation" {
  // 测试资源分配场景
  let total_tasks = 47
  let team_members = 5
  let tasks_per_member = @azimuth.divide_with_ceil(total_tasks, team_members)
  @azimuth.assert_eq(10, tasks_per_member)
  
  let total_pages = 103
  let chapters = 8
  let pages_per_chapter = @azimuth.divide_with_ceil(total_pages, chapters)
  @azimuth.assert_eq(13, pages_per_chapter)
}

test "complex_calculator_operations" {
  // 测试复杂计算器操作
  let x = 17
  let y = 23
  let z = 31
  
  // 计算 (x * y) + (x * z)
  let term1 = @azimuth.multiply(x, y)
  let term2 = @azimuth.multiply(x, z)
  let result = @azimuth.add(term1, term2)
  @azimuth.assert_eq(918, result)
  
  // 计算 x * (y + z)
  let sum = @azimuth.add(y, z)
  let result2 = @azimuth.multiply(x, sum)
  @azimuth.assert_eq(918, result2)
}

test "edge_case_zero_operations" {
  // 测试零操作边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999999))
  @azimuth.assert_eq(0, @azimuth.multiply(999999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 999))
}

test "negative_number_comprehensive" {
  // 测试负数综合情况
  let a = -15
  let b = -8
  let c = 12
  
  @azimuth.assert_eq(-23, @azimuth.add(a, b))
  @azimuth.assert_eq(-3, @azimuth.add(a, c))
  @azimuth.assert_eq(120, @azimuth.multiply(a, b))
  @azimuth.assert_eq(-180, @azimuth.multiply(a, c))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(a, 8))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(a, 20))
}

test "large_number_operations" {
  // 测试大数操作
  let large1 = 1000000
  let large2 = 2000000
  let large3 = 3000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large1, large2))
  @azimuth.assert_eq(6000000, @azimuth.add(@azimuth.add(large1, large2), large3))
  @azimuth.assert_eq(2000000, @azimuth.multiply(large1, large2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large1, large2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(large2, large1))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(y, 0))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(y, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(y, 0))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}