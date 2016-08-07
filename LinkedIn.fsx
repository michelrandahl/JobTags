#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "Utils.fs"
#load "JobInformationDefinitions.fs"
#load "JobInformationExtraction.fs"

open FSharp.Data
open System
open Utils
open JobInformationDefinitions
open JobInformationExtraction



[<Literal>]
let json_sample = """
{"applicants":"2","company":{"data":["Information Technology and Services · 201-500 employees · Founded 2000 · Partnership"],"description":"Vi er et it-kompetencehus med ca. 260 medarbejdere på kontorer i Ballerup og Aarhus. Over 150 af vores medarbejdere er konsulenter. De står klar til at hjælpe din organisation, hvad enten det handler om Network, Security, Datacenter, Collaboration eller Services & Operations. Vi er Danmarks største Cisco-partner, og vi har mange års erfaring med at rådgive, designe, implementere og servicere danske organisationers netværk og infrastruktur. I december 2014 blev Axcess A/S købt af Danmarks største leverandør af it-infrastruktur, Atea. To plus to giver mere end fire - det er mantraet i det nye samarbejde. Vores 70 nye kolleger fra Atea vil fremover give dig adgang til endnu flere kompetencer og en serviceorganisation, der dækker i hele Norden. Sammen med Atea bygger vi Danmark med it.Axcess´ mission er at sikre vores kunder en fremtid som aktører i den nye globale netøkonomi. Vi vil bringe vores kunder helt i front i den nye hurtigt voksende globale konkurrence, hvor Internettet og den konvergerede kommunikationsteknologi kommer til at spille en afgørende rolle. Ring til os på +45 70 266 366 eller besøg os på www.axcess.dk","link":"https://www.linkedin.com/company/axcess-as?trk=job_view_topcard_company_name","name":"Axcess A/S"},"day":"24","description":" Systemarkitekt søges til Axcess i Aarhus    Til vores Tools and Implementation gruppe søger vi en Systemarkitekt, som primært skal være med på holdet til at idriftsætte nye kunder og drifte systemerne.       Har du overblikket til at styre vores kunder trygt på plads i vores organisation – så er det dig vi har brug for i Axcess. Vores team af højt motiverede og engagerede kolleger, står klar til at modtage dig og gøre din hverdag spændende og udviklende.   Som systemarkitekt er det vigtigt, at du er i stand til, at balancere vedligehold og udvikling af de eksisterende miljøer for vores kunder. Systemerne er grundstenen i vores leverancer til kunderne, samt vores interne processer. Derfor er det vigtigt, at systemerne er kørende hele tiden, samtidig med at de løbende tilpasses for at gøre arbejdsopgaverne så smidige som muligt.     Det er vigtigt for os, at du kan samarbejde med de forskellige interessenter, men samtidig er selvkørende, når retningen er udstukket.     De primære opgaver som Systemarkitekt vil være:   ·         Oprette nye kunder i de nuværende systemer   ·         Vedligeholde nuværende kundeovervågninger   ·         Vedligeholde Windows og Linux miljø (100+ servere)   ·         VMware miljø drift       Forventninger til dig  For at blive en succes i rollen er det vigtigt, at du formår at arbejde selvstændigt og besidder en robusthed og en evne til at skabe værdi, også når det skal gå stærkt. Vi forestiller os, at du har prøvet ovenstående før i en lignende stilling, eller alternativt at du har en relevant uddannelse som eksempelvis datamatiker. Du har gode samarbejdsevner til at indgå i projektteams og samarbejde på tværs af faggrænser.       Om dig:   Du kan godt lide at arbejde med Windows og Linux Du kan holde mange bolde i luften også selv om det går lidt stærkt Du kan tage et ansvar og holde fast i problemer til de er løst Du har lyst til at lære nye ting Du har en god humor             Du taler og skriver perfekt dansk og engelsk flydende   Du er analytisk stærk, struktureret og har fokus på detaljen        En del af it-branchens bedste? Fokus på medarbejdertrivsel og udvikling af en sund og stærk kultur har givet os en topplacering blandt Danmarks bedste it-arbejdspladser. Axcess skal være det naturlige valg, hvor du som medarbejder udvikler dig sammen med branchens mest kompetente og engagerede kollegaer – vi kalder det The Place To Be. Som en del af Atea er vi Nordens største it-leverandør, og vi er begejstrede for den forskel, vi sammen kan gøre med it.   Vi tilbyder høj faglig udvikling og en uformel kultur, hvor du kan arbejde selvstændigt med frie rammer og masser af albuerum. Derfor skal du have lyst til at gribe det ansvar, du får fra dag ét. Vi håber, du vil være med?       Ansættelsessted   Aarhus       Kontakt og ansøgning   Du kan få yderligere information om stillingen Operations Manager, Rolf Wulff Ljungberg, på telefon 4214 0484.       Ansøgningsfrist   Snarest muligt. Vi gør opmærksom på, at vi løbende indkalder til samtale.       Vi ser frem til at høre fra dig!           Læs mere om os på www.axcess.dk og følg os på Facebook og LinkedIn    ","employment":"Full-time","experience":"Mid-Senior level","from":"May 6, 2016","id":131078688,"industry":"Information Technology and Services","jobfunction":"Information Technology","till":"June 5, 2016","title":"Systemarkitekt til Axcess","url":"https://www.linkedin.com/jobs2/view/131078688?refId=2346072921464633161080&trk=vsrp_jobs_res_name&trkInfo=VSRPsearchId%3A2346072921464633161080%2CVSRPtargetId%3A131078688%2CVSRPcmpt%3Aprimary","views":"93"}
"""

type LinkedInJob = JsonProvider<json_sample>


let rec extractBasicJobInfo (linkedin_job_json : string) =
    try
        let linkedin_job = LinkedInJob.Parse linkedin_job_json
        let basic_job_description = {
            title = linkedin_job.Title
            brief_description = None
            url = linkedin_job.Url
            }
        let recruiter = linkedin_job.Company.Name
        let views = linkedin_job.Views
        let description = linkedin_job.Description
        let words = getDescriptionWords description
        let extracted_meta_info = {
            recruiter = linkedin_job.Company.Name
            posted = linkedin_job.From
            closes = linkedin_job.Till
            industry = Some linkedin_job.Industry
            linkedin_views = Some linkedin_job.Views
            job_function = Some linkedin_job.Jobfunction
            location = None
            }
        let derived_meta_info = {
            category1_tags = getTags language_keywords words
            category2_tags = getTags other_keywords words
            rare_words = []
            }
        let job_posting = {
            basic_description = basic_job_description
            long_description = description
            extracted_meta_info = extracted_meta_info
            derived_meta_info = derived_meta_info
            }
        Right job_posting
    with e ->
        sprintf "%A" e
        |> functionFailWith <@ extractBasicJobInfo @>

extractBasicJobInfo json_sample
|> printfn "%A\n"

