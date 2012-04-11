package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCommonMetadataAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCommonMetadataResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class FindAllCommonMetadataActionHandler extends AbstractActionHandler<FindAllCommonMetadataAction, FindAllCommonMetadataResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCommonMetadataActionHandler() {
        super(FindAllCommonMetadataAction.class);
    }

    @Override
    public FindAllCommonMetadataResult execute(FindAllCommonMetadataAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> commonMetadataList = metamacCoreCommonService.findAllCommonMetadata(ServiceContextHelper.getServiceContext());
        return new FindAllCommonMetadataResult(commonMetadataList);
    }

    @Override
    public void undo(FindAllCommonMetadataAction action, FindAllCommonMetadataResult result, ExecutionContext context) throws ActionException {

    }

}
