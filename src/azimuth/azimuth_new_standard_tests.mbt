// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(307, total)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列 1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, sum)
}

test "inventory_packaging_optimization" {
  // 库存包装优化计算
  let total_items = 127
  let box_capacity = 15
  let layers_per_box = 3
  
  // 计算需要的盒子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算总层数
  let total_layers = multiply(boxes_needed, layers_per_box)
  
  // 计算每层平均物品数
  let items_per_layer = divide_with_ceil(total_items, total_layers)
  
  assert_eq(9, boxes_needed)
  assert_eq(27, total_layers)
  assert_eq(5, items_per_layer)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 时差8小时
  
  // 计算远程时间（使用24小时制）
  let remote_time_raw = add(local_time, time_difference)
  let remote_time = if remote_time_raw >= 24 {
    add(remote_time_raw, -24)
  } else {
    remote_time_raw
  }
  
  assert_eq(22, remote_time)
}

test "resource_allocation_algorithm" {
  // 资源分配算法
  let total_resources = 500
  let project_requirements = [120, 80, 150, 90]
  
  // 计算总需求
  let total_required = add(add(add(project_requirements[0], project_requirements[1]), project_requirements[2]), project_requirements[3])
  
  // 计算剩余资源
  let remaining_resources = add(total_resources, -total_required)
  
  // 计算可以支持的小项目数量（每个小项目需要20个资源）
  let small_projects = divide_with_ceil(remaining_resources, 20)
  
  assert_eq(440, total_required)
  assert_eq(60, remaining_resources)
  assert_eq(3, small_projects)
}

test "data_structure_simulation" {
  // 数据结构模拟：计算数组统计信息
  let data = [15, 22, 8, 19, 12]
  let data_size = 5
  
  // 计算总和
  let sum = add(add(add(add(data[0], data[1]), data[2]), data[3]), data[4])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, data_size)
  
  // 计算最大值（简化版本）
  let max_value = add(add(add(add(data[0], 
    if data[1] > data[0] { data[1] - data[0] } else { 0 }), 
    if data[2] > data[0] { data[2] - data[0] } else { 0 }),
    if data[3] > data[0] { data[3] - data[0] } else { 0 }),
    if data[4] > data[0] { data[4] - data[0] } else { 0 })
  
  assert_eq(76, sum)
  assert_eq(16, average)
  assert_eq(22, max_value)
}