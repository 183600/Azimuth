// 标准 MoonBit 测试用例 - 新增 10 个测试用例
// 使用标准的 MoonBit 测试语法

test "mathematical_commutativity" {
  // 测试数学交换律
  assert_eq(add(15, 25), add(25, 15))
  assert_eq(multiply(7, 8), multiply(8, 7))
}

test "business_calculation" {
  // 业务计算测试：计算订单总价
  let unit_price = 45
  let quantity = 8
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(360, subtotal)
  assert_eq(372, total_before_discount)
  assert_eq(352, final_total)
}

test "time_management" {
  // 时间管理测试：计算项目工时
  let daily_hours = 8
  let work_days = 12
  let overtime_hours = 6
  
  let total_regular_hours = multiply(daily_hours, work_days)
  let total_hours = add(total_regular_hours, overtime_hours)
  
  assert_eq(96, total_regular_hours)
  assert_eq(102, total_hours)
}

test "resource_allocation" {
  // 资源分配测试：计算需要的容器数量
  let total_items = 89
  let container_capacity = 15
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(6, containers_needed)
}

test "statistical_analysis" {
  // 统计分析测试：计算平均值
  let values_sum = add(add(add(25, 30), 35), 40)
  let count = 4
  let average = divide_with_ceil(values_sum, count)
  
  assert_eq(130, values_sum)
  assert_eq(33, average)
}

test "string_concatenation" {
  // 字符串连接测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
}

test "financial_calculation" {
  // 金融计算测试：计算复利
  let principal = 1000
  let rate_percent = 5
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1050, year1_amount)
  assert_eq(1103, year2_amount)
  assert_eq(1158, year3_amount)
}

test "geometry_calculation" {
  // 几何计算测试：计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(96, area)
  assert_eq(40, perimeter)
}

test "error_handling" {
  // 错误处理测试：除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}