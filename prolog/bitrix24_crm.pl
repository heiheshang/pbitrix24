:- module(bitrix24_crm, [
             lead_add/3,
             lead_add/4,
             lead_get/3,
             lead_get/4,
             lead_list/3,
             lead_list/4,
             lead_update/4,
             lead_update/5,
             activity_add/3,
             activity_add/4,
             activity_list/3,
             activity_list/4,
             contact_add/3,
             contact_add/4,
             deal_list/3,
             deal_list/4,
             contact_list/3,
             contact_list/4,
             duplicate_find_by_comm/3,
             duplicate_find_by_comm/4,
             lead_contact_add/4,
             lead_contact_add/5,
             lead_contact_items_get/3,
             lead_contact_items_get/4,
             lead_productrows_set/4,
             lead_productrows_set/5,
             product_list/3,
             product_list/4
         ]).

:- use_module(bitrix24_rest).

lead_add(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.add', Params, Result).

lead_add(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.add', Params, Result).

lead_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.get', Params, Result).

lead_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.get', Params, Result).

lead_list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.list', Params, Result).

lead_list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.list', Params, Result).

lead_update(Provider, Id, Fields, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.update', [id=Id, fields=Fields], Result).

lead_update(Provider, ContextRef, Id, Fields, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.update', [id=Id, fields=Fields], Result).

activity_add(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.activity.add', Params, Result).

activity_add(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.activity.add', Params, Result).

activity_list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.activity.list', Params, Result).

activity_list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.activity.list', Params, Result).

contact_add(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.contact.add', Params, Result).

contact_add(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.contact.add', Params, Result).

deal_list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.deal.list', Params, Result).

deal_list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.deal.list', Params, Result).

contact_list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.contact.list', Params, Result).

contact_list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.contact.list', Params, Result).

duplicate_find_by_comm(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.duplicate.findbycomm', Params, Result).

duplicate_find_by_comm(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.duplicate.findbycomm', Params, Result).

lead_contact_add(Provider, Id, Fields, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.contact.add', [id=Id, fields=Fields], Result).

lead_contact_add(Provider, ContextRef, Id, Fields, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.contact.add',
                           [id=Id, fields=Fields], Result).

lead_contact_items_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.contact.items.get', Params, Result).

lead_contact_items_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.contact.items.get', Params, Result).

lead_productrows_set(Provider, Id, Rows, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.lead.productrows.set', [id=Id, rows=Rows], Result).

lead_productrows_set(Provider, ContextRef, Id, Rows, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.lead.productrows.set',
                           [id=Id, rows=Rows], Result).

product_list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'crm.product.list', Params, Result).

product_list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'crm.product.list', Params, Result).
