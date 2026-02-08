// Azimuth 项目新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "array_sum_simulation" {
  // 数组求和模拟测试
  // 模拟对数组 [1, 2, 3, 4, 5] 求和
  
  let sum1 = 1
  let sum2 = add(sum1, 2)
  let sum3 = add(sum2, 3)
  let sum4 = add(sum3, 4)
  let sum5 = add(sum4, 5)
  
  assert_eq(sum5, 15)
}

test "percentage_calculation" {
  // 百分比计算测试
  let total = 200
  let percentage = 25
  
  // 计算 total 的 percentage%
  let result = divide_with_ceil(multiply(total, percentage), 100)
  
  assert_eq(result, 50)
}

test "speed_distance_time" {
  // 速度-距离-时间关系测试
  let distance = 150
  let time = 3
  
  // 计算速度 = 距离 / 时间
  let speed = divide_with_ceil(distance, time)
  
  // 验证：距离 = 速度 × 时间
  let calculated_distance = multiply(speed, time)
  
  assert_eq(speed, 50)
  assert_eq(calculated_distance, 150)
}

test "compound_growth_simple" {
  // 简单复合增长测试
  let initial = 1000
  let growth_rate = 10
  let periods = 3
  
  // 第一期增长
  let period1 = add(initial, divide_with_ceil(multiply(initial, growth_rate), 100))
  
  // 第二期增长
  let period2 = add(period1, divide_with_ceil(multiply(period1, growth_rate), 100))
  
  // 第三期增长
  let period3 = add(period2, divide_with_ceil(multiply(period2, growth_rate), 100))
  
  assert_eq(period1, 1100)
  assert_eq(period2, 1210)
  assert_eq(period3, 1331)
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_items = 95
  let batch_size = 10
  
  // 计算需要的批次数
  let batches = divide_with_ceil(total_items, batch_size)
  
  // 最后一批次的项目数
  let last_batch_size = subtract(total_items, multiply(batches - 1, batch_size))
  
  assert_eq(batches, 10)
  assert_eq(last_batch_size, 5)
}

test "area_calculations" {
  // 面积计算测试
  let length = 8
  let width = 6
  
  // 矩形面积
  let rectangle_area = multiply(length, width)
  
  // 三角形面积（假设底和高与矩形相同）
  let triangle_area = divide_with_ceil(rectangle_area, 2)
  
  // 梯形面积（假设上底为4，下底为8，高为6）
  let trapezoid_area = divide_with_ceil(multiply(add(4, 8), 6), 2)
  
  assert_eq(rectangle_area, 48)
  assert_eq(triangle_area, 24)
  assert_eq(trapezoid_area, 36)
}

test "currency_conversion" {
  // 货币转换测试
  let amount_usd = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  
  // 转换为人民币
  let amount_cny = multiply(amount_usd, exchange_rate)
  
  // 包含手续费的转换（假设手续费3%）
  let fee = divide_with_ceil(multiply(amount_cny, 3), 100)
  let final_amount = subtract(amount_cny, fee)
  
  assert_eq(amount_cny, 700)
  assert_eq(fee, 21)
  assert_eq(final_amount, 679)
}

test "inventory_reorder_point" {
  // 库存重新订购点测试
  let daily_demand = 20
  let lead_time = 5  // 供货周期（天）
  let safety_stock = 15
  
  // 计算重新订购点
  let reorder_point = add(multiply(daily_demand, lead_time), safety_stock)
  
  // 计算最大库存水平（假设订购量为200）
  let order_quantity = 200
  let max_inventory = add(reorder_point, order_quantity)
  
  assert_eq(reorder_point, 115)
  assert_eq(max_inventory, 315)
}