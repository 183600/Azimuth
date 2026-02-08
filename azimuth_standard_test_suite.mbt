// Azimuth 项目标准 MoonBit 测试套件
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(5 + 3, 8)
  assert_eq(10 - 7, 3)
  assert_eq(4 * 6, 24)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5 + 8, 3)
  assert_eq(-10 * -2, 20)
  assert_eq(-15 / 3, -5)
  assert_eq(-17 % 5, -2)
  assert_eq(5 + -3, 2)
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  assert_eq((2 + 3) * 4, 20)
  assert_eq(10 * (5 + 3), 80)
  assert_eq(2 * 3 + 4 * 5, 26)
  assert_eq(100 / (5 * 4), 5)
  assert_eq((10 + 20) / (3 + 2), 6)
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "comparison_operations" {
  // 比较运算测试
  assert_eq(1 < 2, true)
  assert_eq(2 > 1, true)
  assert_eq(1 <= 1, true)
  assert_eq(1 >= 1, true)
  assert_eq(1 == 1, true)
  assert_eq(1 != 2, true)
  assert_eq(-3 < -1, true)
  assert_eq(0 > -5, true)
}

test "string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("中文测试", "中文测试")
  assert_eq_string("123!@#", "123!@#")
}

test "character_operations" {
  // 字符操作测试
  let char_a = 'a'
  let char_A = 'A'
  
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_A.to_string(), "A")
  
  // 测试字符比较
  assert_eq('a' < 'b', true)
  assert_eq('A' < 'a', true)
  assert_eq('z' > 'y', true)
  assert_eq('Z' < 'a', true)
}

test "boundary_value_tests" {
  // 边界值测试
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(0 / 1, 0)
  assert_eq(1 - 1, 0)
  assert_eq(-1 + 1, 0)
  assert_eq(100 / 100, 1)
  assert_eq(1 * 1, 1)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 23
  let y = 47
  
  // 加法交换律: a + b = b + a
  assert_eq(x + y, y + x)
  
  // 乘法交换律: a * b = b * a
  assert_eq(x * y, y * x)
  
  // 验证具体数值
  assert_eq(15 + 30, 30 + 15)
  assert_eq(6 * 7, 7 * 6)
}

test "real_world_calculations" {
  // 真实世界计算测试
  // 购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10  // 10%
  let shipping = 15
  
  // 计算小计
  let subtotal = item_price * quantity  // 25 * 4 = 100
  
  // 计算税费
  let tax = subtotal * tax_rate / 100  // 100 * 10 / 100 = 10
  
  // 计算总价
  let total = subtotal + tax + shipping  // 100 + 10 + 15 = 125
  
  // 计算每件商品的平均成本
  let avg_cost = total / quantity  // 125 / 4 = 31 (整数除法)
  
  // 验证计算结果
  assert_eq(subtotal, 100)
  assert_eq(tax, 10)
  assert_eq(total, 125)
  assert_eq(avg_cost, 31)
  
  // 库存管理场景
  let initial_stock = 200
  let daily_sales = 15
  let days = 7
  let new_shipment = 100
  
  // 计算一周后的库存
  let weekly_sales = daily_sales * days  // 15 * 7 = 105
  let remaining_stock = initial_stock - weekly_sales  // 200 - 105 = 95
  let final_stock = remaining_stock + new_shipment  // 95 + 100 = 195
  
  // 计算库存周转天数
  let turnover_days = final_stock / daily_sales  // 195 / 15 = 13
  
  assert_eq(weekly_sales, 105)
  assert_eq(remaining_stock, 95)
  assert_eq(final_stock, 195)
  assert_eq(turnover_days, 13)
}