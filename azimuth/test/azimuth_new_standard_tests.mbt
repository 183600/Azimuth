// Azimuth 项目新增标准测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能和实际应用场景

// 导入必要的包
test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度处理
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(11, 2))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
  
  // 结合律验证
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(x, y), 5),
    @azimuth.add(x, @azimuth.add(y, 5))
  )
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  let total_packages = 127
  let packages_per_pallet = 24
  let pallets_needed = @azimuth.divide_with_ceil(total_packages, packages_per_pallet)
  
  @azimuth.assert_eq(6, pallets_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(pallets_needed, packages_per_pallet)
  @azimuth.assert_eq(144, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_packages)
  @azimuth.assert_eq(17, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 25000
  let monthly_payment = 1200
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  
  @azimuth.assert_eq(21, months_needed)
  
  // 计算总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(25200, total_payment)
  
  // 计算最后一个月的实际支付金额
  let final_payment = @azimuth.subtract(
    total_payment, 
    @azimuth.multiply(months_needed - 1, monthly_payment)
  )
  @azimuth.assert_eq(1200, final_payment)
}

test "complex_workflow_pipeline" {
  // 复杂工作流流水线测试
  // 场景：生产计划计算
  let daily_production_target = 500
  let machines = 8
  let hours_per_day = 8
  let machine_efficiency = 75  // 75%
  
  // 计算理论产能
  let theoretical_capacity = @azimuth.multiply(machines, @azimuth.multiply(hours_per_day, 10))
  @azimuth.assert_eq(640, theoretical_capacity)
  
  // 计算实际产能
  let actual_capacity = @azimuth.divide_with_ceil(
    @azimuth.multiply(theoretical_capacity, machine_efficiency), 
    100
  )
  @azimuth.assert_eq(480, actual_capacity)
  
  // 计算需要的额外班次
  let additional_shifts = @azimuth.divide_with_ceil(
    @azimuth.subtract(daily_production_target, actual_capacity),
    @azimuth.divide_with_ceil(actual_capacity, 2)
  )
  @azimuth.assert_eq(1, additional_shifts)
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 大数测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：会议室分配优化
  let total_attendees = 156
  let small_room_capacity = 8
  let large_room_capacity = 20
  let available_large_rooms = 4
  
  // 优先使用大房间
  let large_rooms_needed = @azimuth.divide_with_ceil(
    @azimuth.divide_with_ceil(total_attendees, 2), 
    large_room_capacity
  )
  @azimuth.assert_eq(4, large_rooms_needed)
  
  // 验证大房间是否足够
  @azimuth.assert_true(large_rooms_needed <= available_large_rooms)
  
  // 计算大房间可容纳人数
  let large_room_capacity_total = @azimuth.multiply(large_rooms_needed, large_room_capacity)
  @azimuth.assert_eq(80, large_room_capacity_total)
  
  // 计算剩余需要分配的人数
  let remaining_attendees = @azimuth.subtract(total_attendees, large_room_capacity_total)
  @azimuth.assert_eq(76, remaining_attendees)
  
  // 计算需要的小房间数量
  let small_rooms_needed = @azimuth.divide_with_ceil(remaining_attendees, small_room_capacity)
  @azimuth.assert_eq(10, small_rooms_needed)
}

test "data_analysis_computation" {
  // 数据分析计算测试
  // 场景：统计分析计算
  let data_points = 1000
  let sample_size = 50
  let confidence_level = 95  // 95%
  
  // 计算需要的样本数量
  let samples_needed = @azimuth.divide_with_ceil(
    @azimuth.multiply(data_points, confidence_level), 
    100
  )
  @azimuth.assert_eq(950, samples_needed)
  
  // 计算采样轮次
  let sampling_rounds = @azimuth.divide_with_ceil(samples_needed, sample_size)
  @azimuth.assert_eq(19, sampling_rounds)
  
  // 计算实际采样数量
  let actual_samples = @azimuth.multiply(sampling_rounds, sample_size)
  @azimuth.assert_eq(950, actual_samples)
  
  // 计算采样覆盖率
  let coverage_rate = @azimuth.divide_with_ceil(
    @azimuth.multiply(actual_samples, 100), 
    data_points
  )
  @azimuth.assert_eq(95, coverage_rate)
}