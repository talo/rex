use chrono::{TimeDelta, Utc};
use rex::engine::{engine::Builder, program::Program};
use rex_type_system::types::{ADTVariant, Type, ADT};
use std::collections::BTreeMap;
use std::sync::Arc;

#[tokio::test]
pub async fn benchmark_num_constructors() {
    let mut adt_count = 0;
    while adt_count <= 100 {
        let mut builder: Builder<()> = Builder::with_prelude().unwrap();
        let adts = generate_adts(Params {
            adts: adt_count,
            variants: 1,
            fields: 0,
        });
        for adt in adts.iter() {
            builder.register_adt(adt, None, None);
        }
        let t1 = Utc::now();
        let program = Program::compile(builder, "0").unwrap();
        let entry_count = program.ftable.0.len();
        let t2 = Utc::now();
        let compile_time: TimeDelta = t2 - t1;
        println!(
            "compile_time with {} adts, {} entries = {} ms",
            adt_count,
            entry_count,
            compile_time.num_milliseconds()
        );

        adt_count += 10;
    }
}

#[tokio::test]
pub async fn benchmark_num_fields() {
    let mut field_count = 0;
    while field_count <= 20 {
        // FIXME: This is slow. Should be able to handle 100 easily.
        let mut builder: Builder<()> = Builder::with_prelude().unwrap();
        let adts = generate_adts(Params {
            adts: 100,
            variants: 1,
            fields: field_count,
        });
        for adt in adts.iter() {
            builder.register_adt(adt, None, None);
        }
        let t1 = Utc::now();
        let program = Program::compile(builder, "0").unwrap();
        let entry_count = program.ftable.0.len();
        let t2 = Utc::now();
        let compile_time: TimeDelta = t2 - t1;
        println!(
            "compile_time with {} fields, {} entries = {} ms",
            field_count,
            entry_count,
            compile_time.num_milliseconds()
        );

        field_count += 10;
    }
}

#[tokio::test]
async fn benchmark_simple() {
    let mut builder: Builder<()> = Builder::with_prelude().unwrap();
    let adts = generate_adts(Params {
        adts: 2,
        variants: 2,
        fields: 2,
    });
    for adt in adts.iter() {
        builder.register_adt(adt, None, None);
    }
    for adt in adts.iter() {
        println!("{}", adt);
    }

    let t1 = Utc::now();
    // println!("{:#?}", adts);
    let program = Program::compile(
        builder,
        r#"
        let
            a0 = ADT0_Variant0 { field0 = 3, field1 = 4 },
            a1 = ADT0_Variant1 { field0 = 5, field1 = 6 }
        in
            [a0, a1]
        "#,
    )
    .unwrap();

    let t2 = Utc::now();
    let compile_time: TimeDelta = t2 - t1;
    println!("compile_time = {} ms", compile_time.num_milliseconds());

    let res = program.run(()).await.unwrap();
    let t3 = Utc::now();

    let eval_time: TimeDelta = t3 - t2;
    println!("eval_time = {} ms", eval_time.num_milliseconds());

    println!();
    println!("res = {}", res);
}

struct Params {
    adts: usize,
    variants: usize,
    fields: usize,
}

fn generate_adts(mut params: Params) -> Vec<Arc<Type>> {
    params.variants = std::cmp::max(1, params.variants);
    let mut result: Vec<Arc<Type>> = Vec::new();
    for adt_no in 0..params.adts {
        let mut adt = ADT {
            name: format!("ADT{}", adt_no),
            variants: Vec::new(),
            docs: None,
        };
        for variant_no in 0..params.variants {
            let variant_name = if params.variants > 1 {
                format!("ADT{}_Variant{}", adt_no, variant_no)
            } else {
                format!("ADT{}", adt_no)
            };

            if params.fields == 0 {
                adt.variants.push(ADTVariant {
                    name: variant_name,
                    t: None,
                    docs: None,
                    t_docs: None,
                });
            } else {
                let mut entries: BTreeMap<String, Arc<Type>> = BTreeMap::new();

                for field_no in 0..params.fields {
                    entries.insert(format!("field{}", field_no), Arc::new(Type::Uint));
                }
                adt.variants.push(ADTVariant {
                    name: variant_name,
                    t: Some(Arc::new(Type::Dict(entries))),
                    docs: None,
                    t_docs: None,
                });
            }
        }
        result.push(Arc::new(Type::ADT(adt)));
    }
    result
}
