use std::{
    fs::{self, File, OpenOptions},
    io,
    marker::PhantomData,
    path::PathBuf,
};

use rex_ast::{
    adt,
    types::{Type, TypeInfo},
};
use uuid::Uuid;

#[cfg_attr(feature = "graphql", derive(async_graphql::Enum))]
#[cfg_attr(feature = "lsor", derive(lsor::Filter, lsor::Type), lsor("==", "!="))]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub enum ObjectFormat {
    #[default]
    #[cfg_attr(feature = "serde", serde(rename = "json"))]
    #[cfg_attr(feature = "graphql", graphql(name = "json"))]
    Json,
    #[cfg_attr(feature = "serde", serde(rename = "bin"))]
    #[cfg_attr(feature = "graphql", graphql(name = "bin"))]
    Bin,
}

impl TypeInfo for ObjectFormat
{
    fn t() -> Type {
        Type::ADT(adt! {
            ObjectFormat = Json | Bin
        })
    }
}

fn default_object_format() -> ObjectFormat {
    ObjectFormat::Json
}

#[cfg_attr(feature = "graphql", derive(async_graphql::SimpleObject))]
#[cfg_attr(
    feature = "lsor",
    derive(lsor::Filter, lsor::Row, lsor::Sort),
    lsor(json)
)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VirtualObject {
    pub path: String,
    pub size: u64,

    #[cfg_attr(feature = "lsor", lsor(skip_sort))]
    #[cfg_attr(feature = "serde", serde(default = "default_object_format"))]
    pub format: ObjectFormat,
}

impl<T> From<&Object<T>> for VirtualObject {
    fn from(obj: &Object<T>) -> Self {
        Self {
            path: obj.fd.display().to_string(),
            size: obj.f.metadata().map(|m| m.len()).unwrap_or(0),
            format: obj.format,
        }
    }
}

#[derive(Debug)]
pub struct Object<T> {
    _t: PhantomData<T>,
    pub fd: PathBuf,
    pub f: File,
    pub format: ObjectFormat,
}

impl<T> PartialEq for Object<T> {
    fn eq(&self, other: &Self) -> bool {
        VirtualObject::from(self) == VirtualObject::from(other)
    }
}

impl<T> Object<T> {
    pub fn new() -> io::Result<Self> {
        let fd = Uuid::new_v4().to_string();
        let f = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .truncate(true)
            .open(&fd)?;

        let fd = fd.into();
        Ok(Self {
            _t: PhantomData,
            fd,
            f,
            format: ObjectFormat::Json,
        })
    }

    /// Create a a temporary object that will be deleted when the object is dropped
    pub fn temp() -> io::Result<Self> {
        let fd = tempfile::tempdir()?.into_path().join("temp");
        let f = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .truncate(true)
            .open(&fd)?;

        Ok(Self {
            _t: PhantomData,
            fd,
            f,
            format: ObjectFormat::Json,
        })
    }

    pub fn with_fd<U: Into<PathBuf>>(fd: U) -> io::Result<Self> {
        let fd = fd.into();
        let f = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .truncate(false)
            .open(&fd)?;

        Ok(Self {
            _t: PhantomData,
            fd,
            f,
            format: ObjectFormat::Json,
        })
    }

    pub fn with_format(mut self, format: ObjectFormat) -> io::Result<Self> {
        self.format = format;
        Ok(self)
    }

    pub fn delete(self) -> io::Result<()> {
        fs::remove_file(self.fd)
    }

    /// Copy the file to the given path
    pub fn copy(&self, path: &PathBuf) -> io::Result<u64> {
        fs::copy(&self.fd, path)
    }
}

impl<T> TypeInfo for Object<T>
where
    T: TypeInfo,
{
    fn t() -> Type {
        Type::ADT(adt! {
            Object = Object {
                path = Type::Ptr(Box::new(T::t())),
                size = Type::Uint,
                format = Type::Option(Box::new(ObjectFormat::t()))
            }
        })
    }
}

#[cfg(feature = "serde")]
impl<'de, T> serde::Deserialize<'de> for Object<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let obj = VirtualObject::deserialize(deserializer)?;
        let f = OpenOptions::new()
            .create(false)
            .read(true)
            .write(true)
            .truncate(false)
            .open(&obj.path)
            .map_err(|e| {
                D::Error::custom(format!("cannot open {} for reading: {}", &obj.path, e))
            })?;
        Ok(Self {
            _t: PhantomData,
            fd: obj.path.into(),
            f,
            format: obj.format,
        })
    }
}

#[cfg(feature = "serde")]
impl<T> serde::Serialize for Object<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        VirtualObject {
            path: self.fd.display().to_string(),
            size: 0,
            format: self.format,
        }
        .serialize(serializer)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "lsor", derive(lsor::Row, lsor::Sort), lsor(json))]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct TypedVirtualObject<T> {
    pub path: String,
    pub size: u64,

    #[cfg_attr(feature = "lsor", lsor(skip))]
    #[cfg_attr(feature = "serde", serde(skip))]
    pub _t: PhantomData<T>,
}

impl<T> TypeInfo for TypedVirtualObject<T>
where
    T: TypeInfo,
{
    fn t() -> Type {
        Type::ADT(adt! {
            Object = Object {
                path = Type::Ptr(Box::new(T::t())),
                size = Type::Uint,
                format = Type::Option(Box::new(ObjectFormat::t()))
            }
        })
    }
}

#[cfg(all(feature = "graphql", feature = "serde"))]
#[async_graphql::Scalar]
impl<T> async_graphql::ScalarType for TypedVirtualObject<T>
where
    T: Send + Sync,
{
    fn parse(value: async_graphql::Value) -> async_graphql::InputValueResult<Self> {
        use async_graphql::InputType;

        let json_value: serde_json::Value = match serde_json::Value::parse(Some(value)) {
            Ok(json_value) => json_value,
            Err(e) => panic!("{:?}", e),
        };
        Ok(serde_json::from_value(json_value)?)
    }

    fn to_value(&self) -> async_graphql::Value {
        use async_graphql::InputType;

        serde_json::to_value(self).unwrap().to_value()
    }
}
