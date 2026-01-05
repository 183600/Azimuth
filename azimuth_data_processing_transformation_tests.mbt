// Azimuth Data Processing Transformation Tests
// This file contains comprehensive tests for data processing and transformation operations

// Test 1: Data Type Conversion Operations
test "data type conversion operations" {
  // Test string to numeric conversions
  let string_int = "42"
  let string_float = "3.14159"
  let string_bool = "true"
  
  let converted_int = DataTypeConverter::string_to_int(string_int)
  match converted_int {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let converted_float = DataTypeConverter::string_to_float(string_float)
  match converted_float {
    Some(value) => assert_true(Math::abs(value - 3.14159) < 0.00001)
    None => assert_true(false)
  }
  
  let converted_bool = DataTypeConverter::string_to_bool(string_bool)
  match converted_bool {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  // Test numeric to string conversions
  let int_value = 123
  let float_value = 456.789
  let bool_value = false
  
  let int_to_string = DataTypeConverter::int_to_string(int_value)
  assert_eq(int_to_string, "123")
  
  let float_to_string = DataTypeConverter::float_to_string(float_value)
  assert_true(float_to_string.contains("456.789"))
  
  let bool_to_string = DataTypeConverter::bool_to_string(bool_value)
  assert_eq(bool_to_string, "false")
  
  // Test invalid conversions
  let invalid_int = "not_a_number"
  let invalid_int_result = DataTypeConverter::string_to_int(invalid_int)
  match invalid_int_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let invalid_bool = "maybe"
  let invalid_bool_result = DataTypeConverter::string_to_bool(invalid_bool)
  match invalid_bool_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Data Format Transformation
test "data format transformation" {
  // Create test data in different formats
  let json_data = "{\"name\":\"test\",\"value\":42,\"active\":true}"
  let xml_data = "<data><name>test</name><value>42</value><active>true</active></data>"
  let csv_data = "name,value,active\ntest,42,true"
  
  // Test JSON to object transformation
  let json_obj = FormatTransformer::json_to_object(json_data)
  match json_obj {
    Some(obj) => {
      assert_eq(Object::get_string(obj, "name"), Some("test"))
      assert_eq(Object::get_int(obj, "value"), Some(42))
      assert_eq(Object::get_bool(obj, "active"), Some(true))
    }
    None => assert_true(false)
  }
  
  // Test XML to object transformation
  let xml_obj = FormatTransformer::xml_to_object(xml_data)
  match xml_obj {
    Some(obj) => {
      assert_eq(Object::get_string(obj, "name"), Some("test"))
      assert_eq(Object::get_int(obj, "value"), Some(42))
      assert_eq(Object::get_bool(obj, "active"), Some(true))
    }
    None => assert_true(false)
  }
  
  // Test CSV to array transformation
  let csv_array = FormatTransformer::csv_to_array(csv_data)
  match csv_array {
    Some(array) => {
      assert_eq(array.length(), 2) // Header + 1 data row
      assert_eq(array[0].length(), 3) // 3 columns
      assert_eq(array[0][0], "name")
      assert_eq(array[0][1], "value")
      assert_eq(array[0][2], "active")
      assert_eq(array[1][0], "test")
      assert_eq(array[1][1], "42")
      assert_eq(array[1][2], "true")
    }
    None => assert_true(false)
  }
  
  // Test object to JSON transformation
  let test_obj = Object::new()
  Object::set_string(test_obj, "name", "transformed")
  Object::set_int(test_obj, "value", 100)
  Object::set_bool(test_obj, "active", false)
  
  let transformed_json = FormatTransformer::object_to_json(test_obj)
  assert_true(transformed_json.contains("transformed"))
  assert_true(transformed_json.contains("100"))
  assert_true(transformed_json.contains("false"))
}

// Test 3: Data Aggregation and Grouping
test "data aggregation and grouping operations" {
  // Create test data
  let data_points = [
    DataPoint::new("category_a", 10.0, "region_1"),
    DataPoint::new("category_b", 20.0, "region_1"),
    DataPoint::new("category_a", 15.0, "region_2"),
    DataPoint::new("category_c", 25.0, "region_2"),
    DataPoint::new("category_b", 30.0, "region_3"),
    DataPoint::new("category_a", 12.0, "region_3")
  ]
  
  // Test grouping by category
  let grouped_by_category = DataAggregator::group_by(data_points, func(dp) { DataPoint::category(dp) })
  
  assert_eq(grouped_by_category.length(), 3) // 3 categories
  
  let category_a = grouped_by_category.get("category_a")
  match category_a {
    Some(points) => {
      assert_eq(points.length(), 3)
      assert_eq(points[0].value, 10.0)
      assert_eq(points[1].value, 15.0)
      assert_eq(points[2].value, 12.0)
    }
    None => assert_true(false)
  }
  
  // Test aggregation functions
  let category_a_sum = DataAggregator::sum(category_a.unwrap())
  assert_eq(category_a_sum, 37.0)
  
  let category_a_avg = DataAggregator::average(category_a.unwrap())
  assert_true(Math::abs(category_a_avg - 12.3333333333) < 0.00001)
  
  let category_a_max = DataAggregator::max(category_a.unwrap())
  assert_eq(category_a_max, 15.0)
  
  let category_a_min = DataAggregator::min(category_a.unwrap())
  assert_eq(category_a_min, 10.0)
  
  // Test multi-level grouping
  let multi_grouped = DataAggregator::multi_level_group(
    data_points,
    [func(dp) { DataPoint::category(dp) }, func(dp) { DataPoint::region(dp) }]
  )
  
  assert_true(multi_grouped.length() > 0)
}

// Test 4: Data Filtering and Transformation Pipeline
test "data filtering and transformation pipeline" {
  // Create test data
  let raw_data = [
    RawDataItem::new("item_1", 100, "active", "2023-01-01"),
    RawDataItem::new("item_2", 200, "inactive", "2023-01-02"),
    RawDataItem::new("item_3", 150, "active", "2023-01-03"),
    RawDataItem::new("item_4", 50, "pending", "2023-01-04"),
    RawDataItem::new("item_5", 300, "active", "2023-01-05")
  ]
  
  // Create transformation pipeline
  let pipeline = DataPipeline::new()
  
  // Add filter stage
  DataPipeline::add_filter(pipeline, func(item) {
    RawDataItem::status(item) == "active"
  })
  
  // Add transformation stage
  DataPipeline::add_transform(pipeline, func(item) {
    let new_value = RawDataItem::value(item) * 1.1 // Apply 10% increase
    let new_status = "processed"
    RawDataItem::update(item, new_value, new_status)
  })
  
  // Add sorting stage
  DataPipeline::add_sort(pipeline, func(item1, item2) {
    RawDataItem::value(item1) - RawDataItem::value(item2)
  })
  
  // Execute pipeline
  let processed_data = DataPipeline::execute(pipeline, raw_data)
  
  // Verify results
  assert_eq(processed_data.length(), 3) // Only active items
  
  // Check values are transformed (10% increase)
  assert_eq(processed_data[0].value, 110.0) // 100 * 1.1
  assert_eq(processed_data[1].value, 165.0) // 150 * 1.1
  assert_eq(processed_data[2].value, 330.0) // 300 * 1.1
  
  // Check status is updated
  for item in processed_data {
    assert_eq(item.status, "processed")
  }
  
  // Check sorting (ascending by value)
  assert_true(processed_data[0].value <= processed_data[1].value)
  assert_true(processed_data[1].value <= processed_data[2].value)
}

// Test 5: Data Validation and Cleaning
test "data validation and cleaning operations" {
  // Create test data with various issues
  let dirty_data = [
    DirtyDataItem::new("valid_item", 42, "valid@email.com", "2023-01-01"),
    DirtyDataItem::new("", 0, "invalid_email", "2023-13-45"), // Invalid name, email, date
    DirtyDataItem::new("null_value_item", -1, "", "2023-02-30"), // Negative value, empty email, invalid date
    DirtyDataItem::new("valid_item_2", 100, "another@valid.com", "2023-01-15"),
    DirtyDataItem::new("whitespace_item", 50, "  spaced@email.com  ", " 2023-01-20 ") // Whitespace issues
  ]
  
  // Create validation rules
  let validator = DataValidator::new()
  
  DataValidator::add_rule(validator, "name", ValidationRule::non_empty())
  DataValidator::add_rule(validator, "value", ValidationRule::min_value(0))
  DataValidator::add_rule(validator, "email", ValidationRule::email_format())
  DataValidator::add_rule(validator, "date", ValidationRule::date_format())
  
  // Validate data
  let validation_results = DataValidator::validate(validator, dirty_data)
  
  // Check validation results
  assert_eq(validation_results.length(), 5)
  assert_true(validation_results[0].is_valid) // First item is valid
  assert_false(validation_results[1].is_valid) // Second item has multiple issues
  assert_false(validation_results[2].is_valid) // Third item has multiple issues
  assert_true(validation_results[3].is_valid) // Fourth item is valid
  assert_false(validation_results[4].is_valid) // Fifth item has whitespace issues
  
  // Clean data
  let cleaned_data = DataCleaner::clean(dirty_data)
  
  // Verify cleaning results
  assert_eq(cleaned_data.length(), 5)
  
  // Check whitespace trimming
  assert_eq(cleaned_data[4].name, "whitespace_item")
  assert_eq(cleaned_data[4].email, "spaced@email.com")
  assert_eq(cleaned_data[4].date, "2023-01-20")
  
  // Check default values for invalid data
  assert_eq(cleaned_data[1].name, "default_name")
  assert_eq(cleaned_data[1].value, 0) // Default for negative values
  assert_eq(cleaned_data[1].email, "default@example.com")
  assert_eq(cleaned_data[1].date, "2023-01-01") // Default date
}

// Test 6: Data Normalization and Standardization
test "data normalization and standardization operations" {
  // Create test data with different scales
  let raw_measurements = [
    Measurement::new("temperature", 25.0, "celsius"),
    Measurement::new("temperature", 77.0, "fahrenheit"),
    Measurement::new("temperature", 298.15, "kelvin"),
    Measurement::new("pressure", 101325.0, "pascal"),
    Measurement::new("pressure", 1.01325, "bar"),
    Measurement::new("pressure", 760.0, "mmhg")
  ]
  
  // Create normalizer
  let normalizer = DataNormalizer::new()
  
  // Add normalization rules
  DataNormalizer::add_rule(normalizer, "temperature", func(m) {
    match Measurement::unit(m) {
      "celsius" => Measurement::value(m)
      "fahrenheit" => (Measurement::value(m) - 32.0) * 5.0 / 9.0
      "kelvin" => Measurement::value(m) - 273.15
      _ => Measurement::value(m)
    }
  })
  
  DataNormalizer::add_rule(normalizer, "pressure", func(m) {
    match Measurement::unit(m) {
      "pascal" => Measurement::value(m)
      "bar" => Measurement::value(m) * 100000.0
      "mmhg" => Measurement::value(m) * 133.322
      _ => Measurement::value(m)
    }
  })
  
  // Normalize data
  let normalized_data = DataNormalizer::normalize(normalizer, raw_measurements)
  
  // Verify normalization results
  assert_eq(normalized_data.length(), 6)
  
  // Check temperature normalization (all in Celsius)
  assert_true(Math::abs(normalized_data[0].value - 25.0) < 0.01) // Already Celsius
  assert_true(Math::abs(normalized_data[1].value - 25.0) < 0.01) // 77°F = 25°C
  assert_true(Math::abs(normalized_data[2].value - 25.0) < 0.01) // 298.15K = 25°C
  
  // Check pressure normalization (all in Pascal)
  assert_true(Math::abs(normalized_data[3].value - 101325.0) < 1.0) // Already Pascal
  assert_true(Math::abs(normalized_data[4].value - 101325.0) < 1.0) // 1.01325 bar = 101325 Pa
  assert_true(Math::abs(normalized_data[5].value - 101325.0) < 1.0) // 760 mmHg = 101325 Pa
  
  // Test standardization (z-score normalization)
  let values = [10.0, 20.0, 30.0, 40.0, 50.0]
  let standardized_values = DataStandardizer::z_score(values)
  
  assert_eq(standardized_values.length(), 5)
  assert_true(Math::abs(standardized_values[2]) < 0.01) // Mean should be close to 0
  assert_true(Math::abs(standardized_values[0] + 1.264911064) < 0.01) // First value z-score
  assert_true(Math::abs(standardized_values[4] - 1.264911064) < 0.01) // Last value z-score
}

// Test 7: Data Enrichment and Augmentation
test "data enrichment and augmentation operations" {
  // Create base test data
  let base_data = [
    BaseDataItem::new("user_1", "New York", 100.0),
    BaseDataItem::new("user_2", "London", 150.0),
    BaseDataItem::new("user_3", "Tokyo", 200.0),
    BaseDataItem::new("user_4", "Paris", 120.0)
  ]
  
  // Create enrichment sources
  let location_data = [
    ("New York", "USA", "North America", 8419000),
    ("London", "UK", "Europe", 8982000),
    ("Tokyo", "Japan", "Asia", 13929000),
    ("Paris", "France", "Europe", 2141000)
  ]
  
  let category_ranges = [
    ("low", 0.0, 130.0),
    ("medium", 130.0, 170.0),
    ("high", 170.0, 1000.0)
  ]
  
  // Create enricher
  let enricher = DataEnricher::new()
  
  // Add enrichment functions
  DataEnricher::add_function(enricher, "country", func(item) {
    let city = BaseDataItem::location(item)
    match location_data.find(func((loc, _, _, _)) { loc == city }) {
      Some((_, country, _, _)) => Some(StringValue(country))
      None => None
    }
  })
  
  DataEnricher::add_function(enricher, "continent", func(item) {
    let city = BaseDataItem::location(item)
    match location_data.find(func((loc, _, _, _)) { loc == city }) {
      Some((_, _, continent, _)) => Some(StringValue(continent))
      None => None
    }
  })
  
  DataEnricher::add_function(enricher, "population", func(item) {
    let city = BaseDataItem::location(item)
    match location_data.find(func((loc, _, _, _)) { loc == city }) {
      Some((_, _, _, population)) => Some(IntValue(population))
      None => None
    }
  })
  
  DataEnricher::add_function(enricher, "category", func(item) {
    let value = BaseDataItem::value(item)
    match category_ranges.find(func((_, min, max)) { value >= min && value < max }) {
      Some((category, _, _)) => Some(StringValue(category))
      None => None
    }
  })
  
  // Enrich data
  let enriched_data = DataEnricher::enrich(enricher, base_data)
  
  // Verify enrichment results
  assert_eq(enriched_data.length(), 4)
  
  // Check first item (New York)
  assert_eq(EnrichedDataItem::get_string(enriched_data[0], "country"), Some("USA"))
  assert_eq(EnrichedDataItem::get_string(enriched_data[0], "continent"), Some("North America"))
  assert_eq(EnrichedDataItem::get_int(enriched_data[0], "population"), Some(8419000))
  assert_eq(EnrichedDataItem::get_string(enriched_data[0], "category"), Some("low"))
  
  // Check second item (London)
  assert_eq(EnrichedDataItem::get_string(enriched_data[1], "country"), Some("UK"))
  assert_eq(EnrichedDataItem::get_string(enriched_data[1], "continent"), Some("Europe"))
  assert_eq(EnrichedDataItem::get_int(enriched_data[1], "population"), Some(8982000))
  assert_eq(EnrichedDataItem::get_string(enriched_data[1], "category"), Some("medium"))
  
  // Check third item (Tokyo)
  assert_eq(EnrichedDataItem::get_string(enriched_data[2], "country"), Some("Japan"))
  assert_eq(EnrichedDataItem::get_string(enriched_data[2], "continent"), Some("Asia"))
  assert_eq(EnrichedDataItem::get_int(enriched_data[2], "population"), Some(13929000))
  assert_eq(EnrichedDataItem::get_string(enriched_data[2], "category"), Some("high"))
}

// Test 8: Complex Data Transformation Workflows
test "complex data transformation workflows" {
  // Create raw input data
  let raw_sales_data = [
    SalesRecord::new("2023-01-01", "Product A", 100.0, 5, "North", "Online"),
    SalesRecord::new("2023-01-02", "Product B", 150.0, 3, "South", "Retail"),
    SalesRecord::new("2023-01-03", "Product A", 95.0, 8, "East", "Online"),
    SalesRecord::new("2023-01-04", "Product C", 200.0, 2, "West", "Retail"),
    SalesRecord::new("2023-01-05", "Product B", 160.0, 4, "North", "Online")
  ]
  
  // Create complex transformation workflow
  let workflow = DataWorkflow::new()
  
  // Stage 1: Data validation
  DataWorkflow::add_stage(workflow, "validation", func(data) {
    data.filter(func(record) {
      SalesRecord::price(record) > 0 && 
      SalesRecord::quantity(record) > 0 &&
      SalesRecord::date(record).length() == 10
    })
  })
  
  // Stage 2: Calculate derived fields
  DataWorkflow::add_stage(workflow, "derivation", func(data) {
    data.map(func(record) {
      let total_value = SalesRecord::price(record) * SalesRecord::quantity(record).to_float()
      let discount = if total_value > 400.0 { total_value * 0.1 } else { 0.0 }
      let final_value = total_value - discount
      SalesRecord::add_derived_fields(record, total_value, discount, final_value)
    })
  })
  
  // Stage 3: Categorization
  DataWorkflow::add_stage(workflow, "categorization", func(data) {
    data.map(func(record) {
      let category = match SalesRecord::final_value(record) {
        v if v < 200.0 => "Low Value"
        v if v < 500.0 => "Medium Value"
        _ => "High Value"
      }
      SalesRecord::set_category(record, category)
    })
  })
  
  // Stage 4: Aggregation
  DataWorkflow::add_stage(workflow, "aggregation", func(data) {
    let grouped = DataAggregator::group_by(data, func(record) { 
      SalesRecord::product(record) 
    })
    
    grouped.map(func((product, records)) {
      let total_revenue = records.map(func(r) { SalesRecord::final_value(r) }).sum()
      let total_quantity = records.map(func(r) { SalesRecord::quantity(r) }).sum()
      let avg_price = total_revenue / total_quantity.to_float()
      
      AggregatedSalesData::new(product, total_revenue, total_quantity, avg_price)
    })
  })
  
  // Execute workflow
  let final_results = DataWorkflow::execute(workflow, raw_sales_data)
  
  // Verify workflow results
  assert_eq(final_results.length(), 3) // 3 unique products
  
  // Check Product A aggregation
  let product_a = final_results.find(func(data) { AggregatedSalesData::product(data) == "Product A" })
  match product_a {
    Some(data) => {
      assert_eq(AggregatedSalesData::total_quantity(data), 13) // 5 + 8
      assert_true(Math::abs(AggregatedSalesData::total_revenue(data) - 1210.0) < 0.01) // (100*5 + 95*8) - discounts
      assert_true(Math::abs(AggregatedSalesData::avg_price(data) - 93.0769) < 0.01)
    }
    None => assert_true(false)
  }
  
  // Check Product B aggregation
  let product_b = final_results.find(func(data) { AggregatedSalesData::product(data) == "Product B" })
  match product_b {
    Some(data) => {
      assert_eq(AggregatedSalesData::total_quantity(data), 7) // 3 + 4
      assert_true(Math::abs(AggregatedSalesData::total_revenue(data) - 1035.0) < 0.01) // (150*3 + 160*4) - discounts
      assert_true(Math::abs(AggregatedSalesData::avg_price(data) - 147.8571) < 0.01)
    }
    None => assert_true(false)
  }
  
  // Check Product C aggregation
  let product_c = final_results.find(func(data) { AggregatedSalesData::product(data) == "Product C" })
  match product_c {
    Some(data) => {
      assert_eq(AggregatedSalesData::total_quantity(data), 2)
      assert_true(Math::abs(AggregatedSalesData::total_revenue(data) - 400.0) < 0.01) // 200*2 - discount
      assert_true(Math::abs(AggregatedSalesData::avg_price(data) - 200.0) < 0.01)
    }
    None => assert_true(false)
  }
}