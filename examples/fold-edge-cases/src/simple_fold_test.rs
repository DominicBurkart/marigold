use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::main]
async fn main() {
    println!("Testing simple fold only...");
    
    // Test just basic fold with explicit type annotations
    let result = m!(
        fn add(acc: u32, item: u32) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)
            .fold(0, add)
            .return
    ).await;
    
    println!("Result type: {:?}", std::any::type_name_of_val(&result));
    
    let collected: Vec<u32> = result.collect().await;
    println!("Collected result: {:?}", collected);
    assert_eq!(collected, vec![6]);
    
    println!("✅ Simple fold test passed!");
}