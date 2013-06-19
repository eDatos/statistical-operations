package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.criteria.MetamacCriteria;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaDisjunctionRestriction;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaOrder;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaOrder.OrderTypeEnum;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPaginator;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPropertyRestriction.OperationType;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.criteria.FamilyCriteriaOrderEnum;
import org.siemac.metamac.statistical.operations.core.criteria.FamilyCriteriaPropertyEnum;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFamilyPaginatedListActionHandler extends SecurityActionHandler<GetFamilyPaginatedListAction, GetFamilyPaginatedListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetFamilyPaginatedListActionHandler() {
        super(GetFamilyPaginatedListAction.class);
    }

    @Override
    public GetFamilyPaginatedListResult executeSecurityAction(GetFamilyPaginatedListAction action) throws ActionException {
        try {
            MetamacCriteria criteria = new MetamacCriteria();
            criteria.setPaginator(new MetamacCriteriaPaginator());
            criteria.getPaginator().setFirstResult(action.getFirstResult());
            criteria.getPaginator().setMaximumResultSize(action.getMaxResults());
            criteria.getPaginator().setCountTotalResults(true);

            // Order
            MetamacCriteriaOrder order = new MetamacCriteriaOrder();
            order.setType(OrderTypeEnum.DESC);
            order.setPropertyName(FamilyCriteriaOrderEnum.LAST_UPDATED.name());
            List<MetamacCriteriaOrder> criteriaOrders = new ArrayList<MetamacCriteriaOrder>();
            criteriaOrders.add(order);
            criteria.setOrdersBy(criteriaOrders);

            MetamacCriteriaDisjunctionRestriction disjuction = new MetamacCriteriaDisjunctionRestriction();
            if (!StringUtils.isBlank(action.getFamily())) {
                disjuction.getRestrictions().add(new MetamacCriteriaPropertyRestriction(FamilyCriteriaPropertyEnum.CODE.name(), action.getFamily(), OperationType.ILIKE));
                disjuction.getRestrictions().add(new MetamacCriteriaPropertyRestriction(FamilyCriteriaPropertyEnum.TITLE.name(), action.getFamily(), OperationType.ILIKE));
                disjuction.getRestrictions().add(new MetamacCriteriaPropertyRestriction(FamilyCriteriaPropertyEnum.DESCRIPTION.name(), action.getFamily(), OperationType.ILIKE));
                disjuction.getRestrictions().add(new MetamacCriteriaPropertyRestriction(FamilyCriteriaPropertyEnum.ACRONYM.name(), action.getFamily(), OperationType.ILIKE));
            }
            criteria.setRestriction(disjuction);

            MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(ServiceContextHolder.getCurrentServiceContext(), criteria);
            return new GetFamilyPaginatedListResult(result.getResults(), result.getPaginatorResult().getFirstResult(), result.getPaginatorResult().getTotalResults());
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }
}
