// 高质量综合测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  assert_eq(0, add(0, 0))
  assert_eq(10, add(3, 7))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_unicode_and_special" {
  // 测试问候函数的 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, line1
line2!", greet("line1
line2"))
  assert_eq_string("Hello, tab	test!", greet("tab	test"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑计算
  // 场景：计算订单总成本（商品价格 × 数量 + 运费）
  let item_price = 25
  let quantity = 8
  let shipping_cost = 15
  let total_cost = add(multiply(item_price, quantity), shipping_cost)
  assert_eq(215, total_cost)  // 25*8 + 15 = 200 + 15 = 215
  
  // 场景：计算需要的包装盒数量
  let total_items = 37
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(7, boxes_needed)  // ceil(37/6) = 7
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  let z = 5
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "error_handling_and_boundary_values" {
  // 测试错误处理和边界值
  // 除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
  assert_eq(0, add(1000000000, -1000000000))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, email@domain.com!", greet("email@domain.com"))
  assert_eq_string("Hello, (parentheses)!", greet("(parentheses)"))
}

test "nested_calculations_workflow" {
  // 测试嵌套计算工作流
  // 场景：计算多批次货物的总容器需求
  let batch_sizes = [23, 15, 8, 31]
  let container_capacity = 10
  
  let batch1_containers = divide_with_ceil(batch_sizes[0], container_capacity)
  let batch2_containers = divide_with_ceil(batch_sizes[1], container_capacity)
  let batch3_containers = divide_with_ceil(batch_sizes[2], container_capacity)
  let batch4_containers = divide_with_ceil(batch_sizes[3], container_capacity)
  
  let total_containers = add(add(batch1_containers, batch2_containers), 
                             add(batch3_containers, batch4_containers))
  
  assert_eq(10, total_containers)  // 3+2+1+4 = 10
  
  // 复杂嵌套计算验证
  let complex_result = divide_with_ceil(multiply(add(5, 3), 4), 6)
  assert_eq(6, complex_result)  // ceil((5+3)*4/6) = ceil(32/6) = 6
}

test "performance_large_numbers" {
  // 测试大数性能计算
  let large_num1 = 100000
  let large_num2 = 20000
  
  // 大数加法
  assert_eq(120000, add(large_num1, large_num2))
  
  // 大数乘法
  assert_eq(2000000000, multiply(large_num1, large_num2))
  
  // 大数除法
  assert_eq(10, divide_with_ceil(large_num1, 10000))
  assert_eq(11, divide_with_ceil(large_num1, 9500))
  
  // 复合大数计算
  let complex_large = add(multiply(large_num1, 2), divide_with_ceil(large_num2, 1000))
  assert_eq(200020, complex_large)  // 100000*2 + ceil(20000/1000) = 200000 + 20 = 200020
}