// Azimuth 库标准增强测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 涵盖基本功能、边界条件、数学性质和实际应用场景

///|
test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  inspect(add(5, 3), content="8")
  inspect(add(0, 0), content="0")
  inspect(add(-5, 3), content="-2")
  inspect(multiply(4, 6), content="24")
  inspect(multiply(0, 5), content="0")
  inspect(multiply(-3, 7), content="-21")
  inspect(subtract(10, 4), content="6")
  inspect(subtract(5, 10), content="-5")
}

///|
test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 100), content="1")
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(-10, -3), content="4")
  inspect(divide_with_ceil(5, 0), content="0")
  inspect(divide_with_ceil(0, 5), content="0")
}

///|
test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  inspect(add(x, y), content="20")
  inspect(add(y, x), content="20")
  
  // 乘法交换律
  inspect(multiply(x, y), content="96")
  inspect(multiply(y, x), content="96")
  
  // 分配律验证
  let distributive_left = multiply(x, add(y, 2))
  let distributive_right = add(multiply(x, y), multiply(x, 2))
  inspect(distributive_left, content="120")
  inspect(distributive_right, content="120")
}

///|
test "string_greeting_functionality" {
  // 字符串问候功能测试
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet("世界"), content="Hello, 世界!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
  inspect(greet("🚀"), content="Hello, 🚀!")
  inspect(greet("café"), content="Hello, café!")
}

///|
test "business_packaging_calculation" {
  // 业务场景：包装计算测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  inspect(boxes_needed, content="4")
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  inspect(total_capacity, content="48")
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="1")
}

///|
test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 4
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  inspect(step1, content="45")
  inspect(step2, content="7")  // ceil(45/7) = 7
  inspect(result, content="11")  // 7 + 4 = 11
}

///|
test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  inspect(resources_per_team, content="34")
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  inspect(total_allocated, content="102")
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  inspect(utilization, content="98")  // ceil(10000/102) = 98
}

///|
test "real_world_event_planning" {
  // 真实世界场景：活动规划测试
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  inspect(tables_needed, content="9")
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  inspect(available_seats, content="90")
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  inspect(remaining_seats, content="3")
  
  // 验证座位充足性
  let seats_sufficient = available_seats >= participants
  inspect(seats_sufficient, content="true")
}