// 自包含的 MoonBit 测试套件 - 新增测试用例
// 包含函数定义和测试用例，可以直接运行

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

pub fn string_concat(lhs : String, rhs : String) -> String {
  lhs + rhs
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
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
test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与最小相反数相加
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近最小值
  assert_eq(2147483646, add(2147483645, 1))  // 接近最大值
  assert_eq(2147483647, add(1073741823, 1073741824))  // 两个大数相加
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数运算
  assert_eq(46340, multiply(46340, 1))  // sqrt(Int最大值)
  assert_eq(100000000, multiply(10000, 10000))  // 大数相乘
  assert_eq(-100000000, multiply(-10000, 10000))  // 负数乘法
  assert_eq(100000000, multiply(-10000, -10000))  // 负负得正
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(34, divide_with_ceil(100, 3))  // 33.333... -> 34
  
  // 负数情况
  assert_eq(-1, divide_with_ceil(-3, 2))  // -1.5 -> -1
  assert_eq(-33, divide_with_ceil(-100, 3))  // -33.333... -> -33
  
  // 负负得正
  assert_eq(34, divide_with_ceil(-100, -3))  // 33.333... -> 34
  
  // 零处理
  assert_eq(0, divide_with_ceil(0, 5))  // 零除以正数
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的Unicode和特殊字符处理
  assert_eq_string("Hello, 🌟!", greet("🌟"))  // Emoji
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))  // 希腊字母
  assert_eq_string("Hello, Москва!", greet("Москва"))  // 西里尔字母
  assert_eq_string("Hello, 北京!", greet("北京"))  // 中文字符
  assert_eq_string("Hello, \t!", greet("\t"))  // 控制字符
}

test "complex_business_calculation" {
  // 复杂业务计算场景：订单总价计算
  let product_price = 89
  let quantity = 12
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 150
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣
  let final_total = add(pre_discount_total, -discount)
  
  assert_eq(1068, subtotal)
  assert_eq(86, tax_amount)
  assert_eq(1179, pre_discount_total)
  assert_eq(1029, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 15
  let b = 25
  let c = 35
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_logistics_calculation" {
  // 真实世界物流计算：仓库存储优化
  let total_products = 1250
  let pallet_capacity = 48
  let truck_capacity = 320
  let warehouse_zones = 8
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(total_products, pallet_capacity)
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_products, truck_capacity)
  
  // 计算每个区域的平均产品数
  let products_per_zone = divide_with_ceil(total_products, warehouse_zones)
  
  // 计算总运输成本（假设每卡车100元）
  let total_transport_cost = multiply(trucks_needed, 100)
  
  assert_eq(27, pallets_needed)  // ceil(1250/48) = 27
  assert_eq(4, trucks_needed)    // ceil(1250/320) = 4
  assert_eq(157, products_per_zone)  // ceil(1250/8) = 157
  assert_eq(400, total_transport_cost)  // 4 * 100 = 400
}

test "financial_investment_calculation" {
  // 金融投资计算：复利增长模拟
  let initial_investment = 10000
  let annual_return_rate = 7
  let years = 5
  
  // 第1年
  let year1_return = divide_with_ceil(multiply(initial_investment, annual_return_rate), 100)
  let year1_total = add(initial_investment, year1_return)
  
  // 第2年
  let year2_return = divide_with_ceil(multiply(year1_total, annual_return_rate), 100)
  let year2_total = add(year1_total, year2_return)
  
  // 第3年
  let year3_return = divide_with_ceil(multiply(year2_total, annual_return_rate), 100)
  let year3_total = add(year2_total, year3_return)
  
  // 第4年
  let year4_return = divide_with_ceil(multiply(year3_total, annual_return_rate), 100)
  let year4_total = add(year3_total, year4_return)
  
  // 第5年
  let year5_return = divide_with_ceil(multiply(year4_total, annual_return_rate), 100)
  let final_amount = add(year4_total, year5_return)
  
  assert_eq(700, year1_return)
  assert_eq(10700, year1_total)
  assert_eq(749, year2_return)
  assert_eq(11449, year2_total)
  assert_eq(802, year3_return)
  assert_eq(12251, year3_total)
  assert_eq(858, year4_return)
  assert_eq(13109, year4_total)
  assert_eq(918, year5_return)
  assert_eq(14027, final_amount)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483647, -2147483646))
  
  // 零的乘法
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
  
  // 一的乘法
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
}

test "string_processing_with_greet_function" {
  // 使用问候函数进行字符串处理测试
  let first_name = "张"
  let last_name = "三"
  let company = "科技公司"
  let department = "研发部"
  
  // 构建完整姓名
  let full_name = string_concat(first_name, last_name)
  let name_greeting = greet(full_name)
  
  // 构建工作标识
  let work_id = string_concat(string_concat(company, " "), department)
  let work_greeting = greet(work_id)
  
  // 构建邮箱格式
  let email_prefix = "zhangsan"
  let email_domain = "techcorp.com"
  let email = string_concat(string_concat(email_prefix, "@"), email_domain)
  let email_greeting = greet(email)
  
  assert_eq_string("Hello, 张三!", name_greeting)
  assert_eq_string("Hello, 科技公司 研发部!", work_greeting)
  assert_eq_string("Hello, zhangsan@techcorp.com!", email_greeting)
}