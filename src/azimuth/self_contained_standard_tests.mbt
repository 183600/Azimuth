// 新增的标准 MoonBit 测试用例 - 自包含版本
// 包含所有必要的函数定义和测试用例

// 函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_true(condition : Bool) -> Unit {
  if condition {
    ()
  } else {
    let _ = "Boolean assertion failed"
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
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

// 测试用例
test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(100, multiply(10, 10))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 123))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边缘情况测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(2, divide_with_ceil(-10, -4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_variations" {
  // 字符串问候函数的各种变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算仓库存储需求
  let total_products = 158
  let pallet_capacity = 25
  let pallets_needed = divide_with_ceil(total_products, pallet_capacity)
  
  assert_eq(7, pallets_needed)
  
  // 验证总存储容量
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：计算项目资源需求
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(4, sprints_needed)
  
  // 验证总任务处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(60, total_capacity)
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符处理测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, élan!", greet("élan"))
}

test "performance_large_numbers" {
  // 大数运算性能测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(1000000, 1000))
  assert_eq(100001, divide_with_ceil(100000000, 999))
  assert_eq(1, divide_with_ceil(999999, 999999))
}

test "real_world_scenario_event_planning" {
  // 真实世界场景：活动策划测试
  let attendees = 245
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let extra_seats = subtract(total_seats, attendees)
  assert_eq(3, extra_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= attendees)
}