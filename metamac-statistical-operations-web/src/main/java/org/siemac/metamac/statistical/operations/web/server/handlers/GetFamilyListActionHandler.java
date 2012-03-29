package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetFamilyListAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetFamilyListActionHandler extends AbstractActionHandler<GetFamilyListAction, GetFamilyListResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetFamilyListActionHandler() {
        super(GetFamilyListAction.class);
    }

    @Override
    public GetFamilyListResult execute(GetFamilyListAction action, ExecutionContext context) throws ActionException {
        try {
            List<FamilyBaseDto> familyBaseDtos = gopestatServiceFacade.findAllFamilies(ServiceContextHelper.getServiceContext());
            return new GetFamilyListResult(familyBaseDtos);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetFamilyListAction action, GetFamilyListResult result, ExecutionContext context) throws ActionException {

    }

}
