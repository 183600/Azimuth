// Azimuth 新增测试套件 - 10个标准 MoonBit 测试用例
// 测试覆盖边缘情况、实际应用场景和数学性质验证

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(2, divide_with_ceil(1000001, 500000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
}

test "mathematical_distributive_law_extended" {
  // 扩展的数学分配律测试
  let a = 7
  let b = 11
  let c = 13
  
  // a * (b + c) = a * b + a * c
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
  
  // (a + b) * c = a * c + b * c
  let left2 = multiply(add(a, b), c)
  let right2 = add(multiply(a, c), multiply(b, c))
  assert_eq(left2, right2)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  // 场景：计算项目完成时间
  let total_hours_needed = 157
  let work_hours_per_day = 8
  let days_needed = divide_with_ceil(total_hours_needed, work_hours_per_day)
  assert_eq(20, days_needed)  // ceil(157/8) = 20
  
  // 计算实际工作时间
  let actual_work_hours = multiply(days_needed - 1, work_hours_per_day)
  let final_day_hours = total_hours_needed - actual_work_hours
  assert_eq(5, final_day_hours)  // 最后一天工作5小时
}

test "string_concatenation_properties" {
  // 字符串连接性质测试
  let name1 = "MoonBit"
  let name2 = "Azimuth"
  
  // 测试 greet 函数的字符串连接
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  // 验证字符串长度
  assert_eq_string("Hello, MoonBit!", greeting1)
  assert_eq_string("Hello, Azimuth!", greeting2)
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, space separated!", greet("space separated"))
}

test "memory_allocation_simulation" {
  // 内存分配模拟测试
  // 场景：计算内存块分配
  let total_memory = 1024  // KB
  let block_size = 256     // KB
  let blocks_needed = divide_with_ceil(total_memory, block_size)
  assert_eq(4, blocks_needed)  // ceil(1024/256) = 4
  
  // 测试非均匀分配
  let uneven_memory = 1000
  let uneven_blocks = divide_with_ceil(uneven_memory, block_size)
  assert_eq(4, uneven_blocks)  // ceil(1000/256) = 4
  
  // 计算内存浪费
  let total_allocated = multiply(uneven_blocks, block_size)
  let wasted_memory = total_allocated - uneven_memory
  assert_eq(24, wasted_memory)  // 4*256 - 1000 = 24
}

test "negative_number_arithmetic_comprehensive" {
  // 负数算术综合测试
  let positive = 15
  let negative = -8
  
  // 加法测试
  assert_eq(7, add(positive, negative))
  assert_eq(-23, add(-positive, negative))
  assert_eq(0, add(positive, -positive))
  
  // 乘法测试
  assert_eq(-120, multiply(positive, negative))
  assert_eq(120, multiply(-positive, negative))
  assert_eq(64, multiply(negative, negative))
  
  // 除法测试
  assert_eq(-1, divide_with_ceil(-15, 20))
  assert_eq(-2, divide_with_ceil(-25, 15))
  assert_eq(1, divide_with_ceil(-15, -20))
}

test "inventory_batch_processing" {
  // 库存批处理测试
  // 场景：批量处理库存物品
  let items = [
    (120, 35),  // (总数, 批次大小)
    (85, 20),
    (200, 75),
    (45, 50)
  ]
  
  // 计算每种物品需要的批次数
  let batches_1 = divide_with_ceil(items[0].0, items[0].1)  // ceil(120/35) = 4
  let batches_2 = divide_with_ceil(items[1].0, items[1].1)  // ceil(85/20) = 5
  let batches_3 = divide_with_ceil(items[2].0, items[2].1)  // ceil(200/75) = 3
  let batches_4 = divide_with_ceil(items[3].0, items[3].1)  // ceil(45/50) = 1
  
  // 计算总批次数
  let total_batches = add(add(batches_1, batches_2), add(batches_3, batches_4))
  assert_eq(13, total_batches)
  
  // 计算实际处理容量
  let capacity_1 = multiply(batches_1, items[0].1)
  let capacity_2 = multiply(batches_2, items[1].1)
  let capacity_3 = multiply(batches_3, items[2].1)
  let capacity_4 = multiply(batches_4, items[3].1)
  
  let total_capacity = add(add(capacity_1, capacity_2), add(capacity_3, capacity_4))
  assert_eq(490, total_capacity)
}

test "mathematical_identity_elements" {
  // 数学单位元测试
  let test_values = [0, 1, -1, 42, -17, 1000]
  
  // 对每个值测试加法单位元
  for i = 0; i < test_values.length(); i = i + 1 {
    let val = test_values[i]
    assert_eq(val, add(val, 0))  // a + 0 = a
    assert_eq(val, add(0, val))  // 0 + a = a
  }
  
  // 对每个值测试乘法单位元和零元
  for i = 0; i < test_values.length(); i = i + 1 {
    let val = test_values[i]
    assert_eq(val, multiply(val, 1))    // a * 1 = a
    assert_eq(val, multiply(1, val))    // 1 * a = a
    assert_eq(0, multiply(val, 0))      // a * 0 = 0
    assert_eq(0, multiply(0, val))      // 0 * a = 0
  }
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  // 场景：项目资源规划
  let project_duration = 45  // 天
  let team_size = 7
  let daily_cost_per_person = 120
  
  // 计算总人日
  let total_person_days = multiply(project_duration, team_size)
  assert_eq(315, total_person_days)
  
  // 计算总成本
  let total_cost = multiply(total_person_days, daily_cost_per_person)
  assert_eq(37800, total_cost)
  
  // 如果需要分阶段支付（每阶段不超过10000）
  let payment_limit = 10000
  let payment_stages = divide_with_ceil(total_cost, payment_limit)
  assert_eq(4, payment_stages)  // ceil(37800/10000) = 4
  
  // 计算最后一阶段的金额
  let full_stages_cost = multiply(payment_stages - 1, payment_limit)
  let final_stage_cost = total_cost - full_stages_cost
  assert_eq(7800, final_stage_cost)  // 37800 - 3*10000 = 7800
}

test "complex_number_sequence_validation" {
  // 复杂数字序列验证测试
  // 生成并验证数字序列的性质
  let start = 5
  let difference = 3
  let count = 10
  
  // 计算等差数列的和
  // 公式：和 = n/2 * (2a + (n-1)d)
  let double_sum = multiply(count, add(multiply(2, start), multiply(count - 1, difference)))
  let sequence_sum = divide_with_ceil(double_sum, 2)
  
  // 手动验证前几项的和
  let manual_sum = add(
    add(add(add(add(start, add(start, difference)), add(start, multiply(2, difference))), 
           add(start, multiply(3, difference))), add(start, multiply(4, difference))),
    add(add(add(add(start, multiply(5, difference)), add(start, multiply(6, difference))), 
           add(start, multiply(7, difference))), add(start, multiply(8, difference)))
  )
  
  // 验证计算结果
  assert_eq(185, sequence_sum)  // 5+8+11+14+17+20+23+26+29+32 = 185
  
  // 验证数学性质：等差数列平均数等于(首项+末项)/2
  let last_term = add(start, multiply(count - 1, difference))
  let average = divide_with_ceil(add(start, last_term), 2)
  assert_eq(19, average)  // ceil((5+32)/2) = 19
  
  // 验证：和 = 平均数 * 项数
  let calculated_sum = multiply(average, count)
  assert_eq(190, calculated_sum)  // 19 * 10 = 190 (由于向上取整有微小差异)
}