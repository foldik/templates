use rocket::request::Form;
use rocket_contrib::json::Json;

use rocket::State;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(FromForm)]
pub struct Pageable {
    pub page: usize,
    pub limit: usize,
}

#[derive(Serialize, Deserialize)]
pub struct PaginatedList<T> {
    pub page: usize,
    pub limit: usize,
    pub count: usize,
    pub data: Vec<T>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Resource {
    pub id: usize,
    pub name: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct NewResource {
    pub name: String,
}

type Resources = Arc<Mutex<Vec<Resource>>>;

#[get("/resources?<pageable..>")]
pub fn get(
    pageable: Form<Pageable>,
    resources_state: State<Resources>,
) -> Result<Json<PaginatedList<Resource>>, String> {
    let resources = resources_state.lock().unwrap();

    let start_pos = if pageable.page > 0 {
        (pageable.page - 1) * pageable.limit
    } else {
        pageable.page * pageable.limit
    };

    if start_pos < resources.len() {
        Ok(Json(PaginatedList {
            page: pageable.page,
            limit: pageable.limit,
            count: resources.len(),
            data: resources
                .clone()
                .into_iter()
                .skip(start_pos)
                .take(pageable.limit)
                .collect(),
        }))
    } else {
        Err(String::from("Not found"))
    }
}

#[post("/resources", format = "json", data = "<resource_request>")]
pub fn new(
    resource_request: Json<NewResource>,
    resources_state: State<Resources>,
) -> Json<Resource> {
    let mut resources = resources_state.lock().unwrap();
    let id = match resources.last() {
        Some(last_element) => last_element.id + 1,
        None => 1,
    };
    let resource = Resource {
        id: id,
        name: resource_request.name.clone(),
    };
    resources.push(resource.clone());
    Json(resource)
}

#[delete("/resources/<id>")]
pub fn delete(id: usize, resources_state: State<Resources>) -> Result<&'static str, &'static str> {
    let mut resources = resources_state.lock().unwrap();
    resources.retain(|item| item.id != id);

    Ok("Ok")
}