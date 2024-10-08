
open Core.Std

let json_r = "
  {
  \"login\": \"github\",
  \"id\": 1,
  \"url\": \"https://api.github.com/orgs/github\",
  \"repos_url\": \"https://api.github.com/orgs/github/repos\",
  \"events_url\": \"https://api.github.com/orgs/github/events\",
  \"hooks_url\": \"https://api.github.com/orgs/github/hooks\",
  \"issues_url\": \"https://api.github.com/orgs/github/issues\",
  \"members_url\": \"https://api.github.com/orgs/github/members{/member}\",
  \"public_members_url\": \"https://api.github.com/orgs/github/public_members{/member}\",
  \"avatar_url\": \"https://github.com/images/error/octocat_happy.gif\",
  \"description\": \"A great organization\",
  \"name\": \"github\",
  \"company\": \"GitHub\",
  \"blog\": \"https://github.com/blog\",
  \"location\": \"San Francisco\",
  \"email\": \"octocat@github.com\",
  \"public_repos\": 2,
  \"public_gists\": 1,
  \"followers\": 20,
  \"following\": 0,
  \"html_url\": \"https://github.com/octocat\",
  \"created_at\": \"2008-01-14T04:33:35Z\",
  \"type\": \"Organization\",
  \"total_private_repos\": 100,
  \"owned_private_repos\": 100,
  \"private_gists\": 81,
  \"disk_usage\": 10000,
  \"collaborators\": 8,
  \"billing_email\": \"support@github.com\",
  \"plan\": {
    \"name\": \"Medium\",
    \"space\": 400,
    \"private_repos\": 20
  },
  \"default_repository_settings\": \"read\",
  \"members_can_create_repositories\": \"true\"
}
"

let print_org_public_repos str_json =
  let pr = Cpt15_j.org_of_string str_json in
  print_int pr.public_repos

;;
print_org_public_repos json_r
