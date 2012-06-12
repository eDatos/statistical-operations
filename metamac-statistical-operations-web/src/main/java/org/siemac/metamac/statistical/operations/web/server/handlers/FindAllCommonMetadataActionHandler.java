package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCommonMetadataAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCommonMetadataResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindAllCommonMetadataActionHandler extends SecurityActionHandler<FindAllCommonMetadataAction, FindAllCommonMetadataResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCommonMetadataActionHandler() {
        super(FindAllCommonMetadataAction.class);
    }

    @Override
    public FindAllCommonMetadataResult executeSecurityAction(FindAllCommonMetadataAction action) throws ActionException {
        List<ExternalItemBtDto> commonMetadataList = metamacCoreCommonService.findAllCommonMetadata(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllCommonMetadataResult(commonMetadataList);
    }

    @Override
    public void undo(FindAllCommonMetadataAction action, FindAllCommonMetadataResult result, ExecutionContext context) throws ActionException {

    }

}
