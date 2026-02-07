// Azimuth 综合测试套件
// 包含10个标准MoonBit测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(8, subtract(3, -5))
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
}

test "edge_case_division" {
  // 除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial = 100
  let step1 = multiply(initial, 2)  // 200
  let step2 = add(step1, 50)        // 250
  let step3 = subtract(step2, 75)   // 175
  let step4 = divide_with_ceil(step3, 25)  // 7
  
  assert_eq(200, step1)
  assert_eq(250, step2)
  assert_eq(175, step3)
  assert_eq(7, step4)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let discount = 30
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 应用折扣
  let after_discount = subtract(subtotal, discount)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  
  // 计算总价
  let total = add(after_discount, tax)
  
  assert_eq(200, subtotal)
  assert_eq(170, after_discount)
  assert_eq(17, tax)
  assert_eq(187, total)
}

test "algorithm_fibonacci_simulation" {
  // 斐波那契算法模拟测试
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)      // 1
  let f3 = add(f2, f1)      // 2
  let f4 = add(f3, f2)      // 3
  let f5 = add(f4, f3)      // 5
  let f6 = add(f5, f4)      // 8
  let f7 = add(f6, f5)      // 13
  let f8 = add(f7, f6)      // 21
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 120
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 42
  
  // 计算已分配资源
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的新项目数量（每个项目需要15个资源）
  let new_projects = divide_with_ceil(remaining, 15)
  
  // 计算资源利用率
  let utilization = divide_with_ceil(multiply(allocated, 100), total_resources)
  
  assert_eq(105, allocated)
  assert_eq(15, remaining)
  assert_eq(1, new_projects)
  assert_eq(88, utilization)
}

test "data_structure_stack_simulation" {
  // 数据结构栈模拟测试
  let stack_size = 0
  let top_element = 0
  
  // push(10)
  let push1 = 10
  let stack_size1 = add(stack_size, 1)
  let top1 = push1
  
  // push(20)
  let push2 = 20
  let stack_size2 = add(stack_size1, 1)
  let top2 = push2
  
  // push(30)
  let push3 = 30
  let stack_size3 = add(stack_size2, 1)
  let top3 = push3
  
  // pop() -> 返回30
  let pop1 = top3
  let stack_size_after_pop1 = subtract(stack_size3, 1)
  let top_after_pop1 = push2
  
  // pop() -> 返回20
  let pop2 = top_after_pop1
  let stack_size_after_pop2 = subtract(stack_size_after_pop1, 1)
  let top_after_pop2 = push1
  
  // pop() -> 返回10
  let pop3 = top_after_pop2
  let stack_size_after_pop3 = subtract(stack_size_after_pop2, 1)
  
  assert_eq(1, stack_size1)
  assert_eq(10, top1)
  assert_eq(2, stack_size2)
  assert_eq(20, top2)
  assert_eq(3, stack_size3)
  assert_eq(30, top3)
  assert_eq(30, pop1)
  assert_eq(2, stack_size_after_pop1)
  assert_eq(20, top_after_pop1)
  assert_eq(20, pop2)
  assert_eq(1, stack_size_after_pop2)
  assert_eq(10, top_after_pop2)
  assert_eq(10, pop3)
  assert_eq(0, stack_size_after_pop3)
}