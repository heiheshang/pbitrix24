:- module(bitrix24_crm, [
             lead_add/2,
             lead_get/2,
             lead_list/2,
             lead_update/3,
             activity_add/2,
             activity_list/2,
             contact_add/2,
             deal_list/2,
             contact_list/2,
             duplicate_find_by_comm/2,
             lead_contact_add/3,
             lead_contact_items_get/2,
             lead_productrows_set/3,
             product_list/2
         ]).

:- use_module(bitrix24_rest).

lead_add(Params, Result) :-
    bitrix24_rest:api_call('crm.lead.add', Params, Result).

lead_get(Params, Result) :-
    bitrix24_rest:api_call('crm.lead.get', Params, Result).

lead_list(Params, Result) :-
    bitrix24_rest:api_call('crm.lead.list', Params, Result).

lead_update(Id, Fields, Result) :-
    bitrix24_rest:api_call('crm.lead.update', [id=Id, fields=Fields], Result).

activity_add(Params, Result) :-
    bitrix24_rest:api_call('crm.activity.add', Params, Result).

activity_list(Params, Result) :-
    bitrix24_rest:api_call('crm.activity.list', Params, Result).

contact_add(Params, Result) :-
    bitrix24_rest:api_call('crm.contact.add', Params, Result).

deal_list(Params, Result) :-
    bitrix24_rest:api_call('crm.deal.list', Params, Result).

contact_list(Params, Result) :-
    bitrix24_rest:api_call('crm.contact.list', Params, Result).

duplicate_find_by_comm(Params, Result) :-
    bitrix24_rest:api_call('crm.duplicate.findbycomm', Params, Result).

lead_contact_add(Id, Fields, Result) :-
    bitrix24_rest:api_call('crm.lead.contact.add', [id=Id, fields=Fields], Result).

lead_contact_items_get(Params, Result) :-
    bitrix24_rest:api_call('crm.lead.contact.items.get', Params, Result).

lead_productrows_set(Id, Rows, Result) :-
    bitrix24_rest:api_call('crm.lead.productrows.set', [id=Id, rows=Rows], Result).

product_list(Params, Result) :-
    bitrix24_rest:api_call('crm.product.list', Params, Result).
