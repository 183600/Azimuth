// 标准 MoonBit 综合测试文件
// 为 azimuth 库提供高质量的测试用例

test "subtract_function_basic" {
  // 测试基本的减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(368, total_before_discount)
  @azimuth.assert_eq(348, final_total)
}

test "inventory_management" {
  // 库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_shipment = @azimuth.subtract(initial_stock, total_sold)
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, stock_before_shipment)
  @azimuth.assert_eq(116, final_stock)
}

test "time_calculation_scenario" {
  // 时间计算场景：项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "large_number_operations" {
  // 大数运算测试
  let big_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "greet_special_characters" {
  // 问候函数特殊字符测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "zero_operations_comprehensive" {
  // 零运算综合测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}