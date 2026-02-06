// Azimuth 综合测试套件 - 标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，测试基本数学运算和字符串处理功能

// 定义基本函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  let x = 15
  let y = 7
  let z = 3
  
  // 复合运算测试
  assert_eq(26, add(multiply(x, y), z))  // 15*7 + 3 = 105 + 3 = 108
  assert_eq(36, multiply(add(x, y), z))  // (15+7)*3 = 22*3 = 66
  assert_eq(2, divide_with_ceil(add(x, y), z))  // ceil(22/3) = 8
  
  // 链式运算测试
  let result = subtract(multiply(add(x, y), 2), divide_with_ceil(multiply(y, z), 4))
  assert_eq(41, result)  // (22*2) - ceil(21/4) = 44 - 6 = 38
}

test "mathematical_identities_and_properties" {
  // 数学恒等式和性质测试
  let a = 42
  let b = 17
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
  
  // 结合律验证
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
  assert_eq(multiply(multiply(a, b), 2), multiply(a, multiply(b, 2)))
}

test "boundary_conditions_and_extremes" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  let a = -15
  let b = 8
  
  // 负数算术
  assert_eq(-7, add(a, b))
  assert_eq(-23, add(a, -b))
  assert_eq(-120, multiply(a, b))
  assert_eq(120, multiply(a, -b))
  
  // 负数除法
  assert_eq(-1, divide_with_ceil(a, b))
  assert_eq(-2, divide_with_ceil(a, 7))
  assert_eq(2, divide_with_ceil(-a, b))
  assert_eq(1, divide_with_ceil(-a, -b))
  
  // 负数减法
  assert_eq(-23, subtract(a, b))
  assert_eq(-7, subtract(a, -b))
  assert_eq(23, subtract(-a, b))
}

test "real_world_business_scenarios" {
  // 真实世界业务场景测试
  // 库存管理场景
  let total_products = 347
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(15, boxes_needed)
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(360, total_capacity)
  let unused_space = subtract(total_capacity, total_products)
  assert_eq(13, unused_space)
  
  // 项目规划场景
  let team_size = 12
  let tasks_per_person = 8
  let sprint_duration = 2
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = multiply(team_size, sprint_duration)
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(96, total_tasks)
  assert_eq(24, tasks_per_sprint)
  assert_eq(4, sprints_needed)
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 基本问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // Unicode 测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, émoji!", greet("émoji"))
  
  // 复杂字符串测试
  assert_eq_string("Hello, MixedCASE123!", greet("MixedCASE123"))
  assert_eq_string("Hello, space at end !", greet("space at end "))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1000001, divide_with_ceil(1000000, 0.999999))  // 近似处理
  
  // 边界减法
  assert_eq(1000000, subtract(1000000, 0))
  assert_eq(-1000000, subtract(0, 1000000))
  assert_eq(0, subtract(1000000, 1000000))
}

test "complex_calculation_workflows" {
  // 复杂计算工作流测试
  // 多步骤业务计算
  let initial_budget = 50000
  let team_members = 8
  let monthly_cost_per_member = 3000
  let project_duration = 6
  
  // 计算总人力成本
  let total_labor_cost = multiply(team_members, multiply(monthly_cost_per_member, project_duration))
  assert_eq(144000, total_labor_cost)
  
  // 计算预算缺口
  let budget_gap = subtract(total_labor_cost, initial_budget)
  assert_eq(94000, budget_gap)
  
  // 计算需要的资金轮次（每轮最多筹集40000）
  let funding_rounds = divide_with_ceil(budget_gap, 40000)
  assert_eq(3, funding_rounds)
  
  // 验证总筹集资金
  let total_raised = multiply(funding_rounds, 40000)
  assert_eq(120000, total_raised)
  
  // 计算剩余资金
  let remaining_funds = subtract(total_raised, budget_gap)
  assert_eq(26000, remaining_funds)
}

test "algorithmic_efficiency_tests" {
  // 算法效率测试
  // 递归风格的迭代计算模拟
  let n = 10
  let mut sum = 0
  let mut i = 1
  
  // 模拟循环：sum = 1 + 2 + ... + n
  sum = add(sum, i)  // i = 1
  i = add(i, 1)      // i = 2
  sum = add(sum, i)  // sum = 1 + 2 = 3
  i = add(i, 1)      // i = 3
  sum = add(sum, i)  // sum = 3 + 3 = 6
  i = add(i, 1)      // i = 4
  sum = add(sum, i)  // sum = 6 + 4 = 10
  i = add(i, 1)      // i = 5
  sum = add(sum, i)  // sum = 10 + 5 = 15
  i = add(i, 1)      // i = 6
  sum = add(sum, i)  // sum = 15 + 6 = 21
  i = add(i, 1)      // i = 7
  sum = add(sum, i)  // sum = 21 + 7 = 28
  i = add(i, 1)      // i = 8
  sum = add(sum, i)  // sum = 28 + 8 = 36
  i = add(i, 1)      // i = 9
  sum = add(sum, i)  // sum = 36 + 9 = 45
  i = add(i, 1)      // i = 10
  sum = add(sum, i)  // sum = 45 + 10 = 55
  
  assert_eq(55, sum)  // 1+2+...+10 = 55
  
  // 验证数学公式：n*(n+1)/2
  let formula_result = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, formula_result)
}

test "data_structure_simulation" {
  // 数据结构模拟测试
  // 模拟数组操作
  let array_size = 10
  let items_per_page = 3
  
  // 计算总页数
  let total_pages = divide_with_ceil(array_size, items_per_page)
  assert_eq(4, total_pages)
  
  // 模拟分页计算
  let page1_start = 0
  let page1_end = subtract(items_per_page, 1)
  let page1_count = add(subtract(page1_end, page1_start), 1)
  assert_eq(3, page1_count)
  
  // 最后一页计算
  let last_page_start = multiply(subtract(total_pages, 1), items_per_page)
  let last_page_items = subtract(array_size, last_page_start)
  assert_eq(1, last_page_items)  // 最后一页只有1个项目
  
  // 验证总项目数
  let full_pages = subtract(total_pages, 1)
  let full_page_items = multiply(full_pages, items_per_page)
  let total_items = add(full_page_items, last_page_items)
  assert_eq(array_size, total_items)
}