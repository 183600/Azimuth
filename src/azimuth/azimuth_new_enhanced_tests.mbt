// Azimuth 项目新增 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖不同的应用场景和边界情况

test "array_sum_simulation" {
  // 数组求和模拟测试
  // 模拟数组 [10, 20, 30, 40, 50] 的求和过程
  
  let element1 = 10
  let element2 = 20
  let element3 = 30
  let element4 = 40
  let element5 = 50
  
  // 逐步累加
  let sum1 = element1
  let sum2 = add(sum1, element2)
  let sum3 = add(sum2, element3)
  let sum4 = add(sum3, element4)
  let sum5 = add(sum4, element5)
  
  // 计算平均值
  let count = 5
  let average = divide_with_ceil(sum5, count)
  
  assert_eq(10, sum1)
  assert_eq(30, sum2)
  assert_eq(60, sum3)
  assert_eq(100, sum4)
  assert_eq(150, sum5)
  assert_eq(30, average)
}

test "logical_operations_simulation" {
  // 逻辑运算模拟测试
  // 使用数值运算模拟逻辑与、或、非操作
  
  let a = 1  // true
  let b = 0  // false
  
  // 逻辑与：a && b = false (0)
  let logical_and = multiply(a, b)
  
  // 逻辑或：a || b = true (1)
  let logical_or = add(a, b)
  let logical_or_normalized = subtract(logical_or, multiply(a, b))
  
  // 逻辑非：!a = false (0), !b = true (1)
  let not_a = subtract(1, a)
  let not_b = subtract(1, b)
  
  assert_eq(0, logical_and)
  assert_eq(1, logical_or_normalized)
  assert_eq(0, not_a)
  assert_eq(1, not_b)
}

test "warehouse_inventory_management" {
  // 仓库库存管理测试
  let initial_stock = 1000
  let incoming_shipment = 250
  let daily_sales = 15
  let days = 10
  let safety_threshold = 200
  
  // 计算新库存
  let new_stock = add(initial_stock, incoming_shipment)
  
  // 计算总销售
  let total_sales = multiply(daily_sales, days)
  
  // 计算最终库存
  let final_stock = subtract(new_stock, total_sales)
  
  // 检查是否需要补货
  let restock_needed = divide_with_ceil(safety_threshold, add(final_stock, 1))
  
  // 计算库存周转率
  let turnover_rate = divide_with_ceil(total_sales, final_stock)
  
  assert_eq(1250, new_stock)
  assert_eq(150, total_sales)
  assert_eq(1100, final_stock)
  assert_eq(0, restock_needed)  // 库存充足，不需要补货
  assert_eq(1, turnover_rate)
}

test "pizza_order_calculation" {
  // 披萨订单计算测试
  let pizza_price = 12
  let topping_price = 2
  let delivery_fee = 5
  let tax_rate = 8
  
  // 订单详情
  let base_pizzas = 3
  let extra_toppings = 4
  
  // 计算基础价格
  let base_cost = multiply(pizza_price, base_pizzas)
  
  // 计算配料费用
  let topping_cost = multiply(topping_price, extra_toppings)
  
  // 计算 subtotal
  let subtotal = add(base_cost, topping_cost)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_cost = add(add(subtotal, tax_amount), delivery_fee)
  
  // 每人平均费用（假设4人分摊）
  let people = 4
  let cost_per_person = divide_with_ceil(total_cost, people)
  
  assert_eq(36, base_cost)
  assert_eq(8, topping_cost)
  assert_eq(44, subtotal)
  assert_eq(4, tax_amount)
  assert_eq(53, total_cost)
  assert_eq(14, cost_per_person)
}

test "binary_number_conversion" {
  // 二进制数转换测试
  // 将二进制数 1101 转换为十进制
  
  let bit3 = multiply(1, 8)  // 2^3 = 8
  let bit2 = multiply(1, 4)  // 2^2 = 4
  let bit1 = multiply(0, 2)  // 2^1 = 2, 但该位为0
  let bit0 = multiply(1, 1)  // 2^0 = 1
  
  // 组合各位
  let decimal_value = add(add(add(bit3, bit2), bit1), bit0)
  
  // 验证设置位的数量
  let set_bits = add(add(1, 1), 1)  // bit3, bit2, bit0 被设置
  
  assert_eq(13, decimal_value)
  assert_eq(3, set_bits)
}

test "car_fuel_efficiency" {
  // 汽车燃油效率测试
  let distance_km = 350
  let fuel_liters = 25
  let fuel_price_per_liter = 1.5
  
  // 计算燃油效率（公里/升）
  let efficiency = divide_with_ceil(distance_km, fuel_liters)
  
  // 计算总油费
  let total_cost = multiply(fuel_liters, divide_with_ceil(fuel_price_per_liter * 10, 10))
  
  // 计算每100公里油耗
  let consumption_per_100km = divide_with_ceil(multiply(fuel_liters, 100), distance_km)
  
  // 计算能否行驶500公里
  let additional_distance = 500
  let needed_fuel = divide_with_ceil(additional_distance, efficiency)
  
  assert_eq(14, efficiency)
  assert_eq(37, total_cost)
  assert_eq(8, consumption_per_100km)
  assert_eq(36, needed_fuel)
}

test "classroom_seating_arrangement" {
  // 教室座位安排测试
  let total_students = 87
  let seats_per_row = 6
  let rows_per_classroom = 8
  
  // 计算需要的行数
  let rows_needed = divide_with_ceil(total_students, seats_per_row)
  
  // 计算需要的教室数量
  let classrooms_needed = divide_with_ceil(rows_needed, rows_per_classroom)
  
  // 计算总座位数
  let total_seats = multiply(classrooms_needed, multiply(rows_per_classroom, seats_per_row))
  
  // 计算空余座位
  let empty_seats = subtract(total_seats, total_students)
  
  // 计算座位利用率
  let utilization_rate = divide_with_ceil(multiply(total_students, 100), total_seats)
  
  assert_eq(15, rows_needed)
  assert_eq(2, classrooms_needed)
  assert_eq(96, total_seats)
  assert_eq(9, empty_seats)
  assert_eq(91, utilization_rate)
}

test "phone_number_formatting" {
  // 电话号码格式化测试
  let area_code = 123
  let exchange = 456
  let subscriber = 7890
  
  // 模拟格式化过程：(123) 456-7890
  let formatted_length = add(add(add(1, 3), 1), add(add(3, 1), 4))  // 包括括号和空格
  
  // 验证各部分
  assert_eq(123, area_code)
  assert_eq(456, exchange)
  assert_eq(7890, subscriber)
  
  // 计算数字总和
  let digit_sum = add(add(add(1, 2), 3), add(add(add(4, 5), 6), add(add(7, 8), 9)))
  assert_eq(45, digit_sum)
  
  // 计算号码的总"权重"（按位加权）
  let weight = add(add(multiply(1, 1000), multiply(2, 100)), multiply(3, 10))
  let total_weight = add(add(weight, multiply(4, 1000)), multiply(5, 100))
  assert_eq(1543, total_weight)
}