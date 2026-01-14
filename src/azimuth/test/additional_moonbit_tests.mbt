// Azimuth 项目附加 MoonBit 测试用例
// 包含10个实用的测试场景，覆盖各种边界情况和实际应用

test "binary_operations_simulation" {
  // 模拟二进制运算：使用加减乘除实现二进制效果
  let a = 16
  let b = 5
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(32, left_shift)
  
  // 右移运算模拟：a >> 2 = a / 4
  let right_shift = a / 4
  @azimuth.assert_eq(4, right_shift)
  
  // 模拟按位与运算（简化）：通过取模运算
  let and_result = @azimuth.multiply(a / b, b)
  @azimuth.assert_eq(15, and_result)
}

test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 500
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  let fee_rate = 2       // 2% 手续费
  
  // 计算兑换金额
  let exchanged_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(3500, exchanged_amount)
  
  // 计算手续费
  let fee = @azimuth.multiply(exchanged_amount, fee_rate) / 100
  @azimuth.assert_eq(70, fee)
  
  // 计算最终到账金额
  let final_amount = @azimuth.add(exchanged_amount, -fee)
  @azimuth.assert_eq(3430, final_amount)
}

test "physics_kinematics_calculation" {
  // 物理运动学计算测试
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 2       // 加速度 m/s²
  let time = 5               // 时间 s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²（简化为整数计算）
  let displacement = @azimuth.add(
    @azimuth.multiply(initial_velocity, time),
    @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2
  )
  @azimuth.assert_eq(75, displacement)
}

test "array_index_calculations" {
  // 数组索引计算测试
  let rows = 4
  let cols = 5
  let row = 2
  let col = 3
  
  // 二维数组转一维数组的索引计算
  let index_1d = @azimuth.add(@azimuth.multiply(row, cols), col)
  @azimuth.assert_eq(13, index_1d)
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  @azimuth.assert_eq(0, first_index)
  
  let last_index = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), @azimuth.add(cols, -1)), @azimuth.add(cols, -1))
  @azimuth.assert_eq(19, last_index)
}

test "date_calculations_simplified" {
  // 简化的日期计算测试
  let year = 2023
  let month = 6
  let day = 15
  
  // 计算一年中的第几天（简化计算，不考虑闰年）
  let days_per_month = 30  // 简化：每月30天
  let day_of_year = @azimuth.add(@azimuth.multiply(month, days_per_month), day)
  @azimuth.assert_eq(195, day_of_year)
  
  // 计算从年初到该日期的总小时数
  let total_hours = @azimuth.multiply(day_of_year, 24)
  @azimuth.assert_eq(4680, total_hours)
}

test "statistical_variance_calculation" {
  // 方差计算测试
  // 数据集：5, 8, 12, 15, 20
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(5, 8), 12), 15), 20)
  let mean = sum / 5
  @azimuth.assert_eq(12, mean)
  
  // 计算方差（简化版）：Σ(x - mean)² / n
  let diff1 = @azimuth.add(5, -12)
  let diff2 = @azimuth.add(8, -12)
  let diff3 = @azimuth.add(12, -12)
  let diff4 = @azimuth.add(15, -12)
  let diff5 = @azimuth.add(20, -12)
  
  let squared_diffs_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1),
    @azimuth.multiply(diff2, diff2)
  ), @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), @azimuth.multiply(diff5, diff5))
  
  let variance = squared_diffs_sum / 5
  @azimuth.assert_eq(26, variance)
}

test "game_inventory_management" {
  // 游戏物品管理计算测试
  let slot_capacity = 20
  let item_stack_size = 64
  let items_total = 150
  
  // 计算需要的槽数
  let slots_needed = @azimuth.divide_with_ceil(items_total, item_stack_size)
  @azimuth.assert_eq(3, slots_needed)
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(slot_capacity, item_stack_size)
  let remaining_space = @azimuth.add(total_capacity, -items_total)
  @azimuth.assert_eq(1130, remaining_space)
  
  // 计算最后一个槽位的物品数量
  let last_slot_items = items_total % item_stack_size
  @azimuth.assert_eq(22, last_slot_items)
}

test "network_data_transfer_calculation" {
  // 网络数据传输计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percent = 10
  
  // 计算基础传输时间（秒）
  let base_time = file_size_mb / network_speed_mbps
  @azimuth.assert_eq(17, base_time)
  
  // 计算开销时间
  let overhead_time = @azimuth.multiply(base_time, overhead_percent) / 100
  @azimuth.assert_eq(1, overhead_time)
  
  // 计算总传输时间
  let total_time = @azimuth.add(base_time, overhead_time)
  @azimuth.assert_eq(18, total_time)
}

test "geometry_circle_properties" {
  // 圆形几何属性计算测试（简化版，π≈3）
  let radius = 8
  
  // 计算直径
  let diameter = @azimuth.multiply(radius, 2)
  @azimuth.assert_eq(16, diameter)
  
  // 计算周长（简化：2 × π × r ≈ 2 × 3 × r）
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(48, circumference)
  
  // 计算面积（简化：π × r² ≈ 3 × r²）
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(192, area)
  
  // 计算球体体积（简化：4/3 × π × r³ ≈ 4 × r³）
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(2048, volume)
}

test "complex_string_validation" {
  // 复杂字符串验证测试
  // 测试各种格式的用户名
  let username1 = "user123"
  let username2 = "test_user"
  let username3 = "admin2023"
  let username4 = ""
  
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet(username1))
  @azimuth.assert_eq_string("Hello, test_user!", @azimuth.greet(username2))
  @azimuth.assert_eq_string("Hello, admin2023!", @azimuth.greet(username3))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(username4))
  
  // 测试包含特殊字符的ID
  let special_id = "ID@2023#001"
  @azimuth.assert_eq_string("Hello, ID@2023#001!", @azimuth.greet(special_id))
  
  // 测试长字符串
  let long_string = "VeryLongUserNameForTestingSystemValidation"
  @azimuth.assert_eq_string("Hello, VeryLongUserNameForTestingSystemValidation!", @azimuth.greet(long_string))
}